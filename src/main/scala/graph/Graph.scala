package graph

import scala.collection.immutable.LongMap
import graph.Tree._
import utils.Lens
import utils.Lens._
import scala.util.control.TailCalls.{done, tailcall, TailRec}

object Graph {

  /*
   * Graph representation based on:
   * Martin Erwig. 2001. Inductive graphs and functional graph algorithms.
   * J. Funct. Program. 11, 5 (September 2001), 467-492.
   */

  // Type alias representing the graph node identifier
  type NodeId = Long

  // Represents a graph node with a generic label type N
  case class Node[N](id: Long, label: N)

  // Represents a directed edge between two nodes with a generic label type E
  case class Edge[E](from: Long, to: Long, label: E) {
    override def toString: String = from.toString ++ " --> " ++ to.toString
  }

  /*
   * Adjacency is a collection of adjacent edges
   * We represent it with a Patricia trie for efficient edge removal and insertion
   */
  type Adjacency[E] = LongMap[E]

  // Represents a node with its incoming and outgoing edges
  case class NodeContext[N, E](node: Node[N], incoming: Adjacency[E], outgoing: Adjacency[E])


  /*
   * Type for a graph.
   * It is simply a wrapper around the internal representation.
   */
  case class Graph[N, E](rep: LongMap[NodeContext[N, E]]) {
    override def toString: String = {
      val ns = nodeIds(this)
      val es = edges(this)
      "\nNodes: " ++ ns.mkString(" - ") ++ "\nEdges: " ++ es.reverse.mkString("\t ")
    }
  }

  // Returns an empty graph
  def empty[N, E]: Graph[N, E] = Graph(rep = LongMap.empty)


  /*
   * Query functions
   */

  // Get the node context associated with a nodeId if it exits in the graph
  def get[N, E](id: NodeId)(graph: Graph[N, E]): Option[NodeContext[N, E]] = graph.rep.get(id)

  // Returns a list of all node Ids in the graph
  def nodeIds[N, E](graph: Graph[N, E]): List[NodeId] = graph.rep.keys.toList

  // Returns a list of all edges in the graph
  def edges[N, E](graph: Graph[N, E]): List[Edge[E]] = {
    def prependEdges(nodeFrom: NodeId)(ctx: NodeContext[N, E]): List[Edge[E]] => List[Edge[E]] = {
      val fn = (nodeTo: Long) => (e: E) => (acc: List[Edge[E]]) => Edge(nodeFrom, nodeTo, e) :: acc
      foldl(fn)(ctx.outgoing)
    }
    foldl(prependEdges)(graph.rep)(Nil)
  }

  // Returns a list of edge tuples
  def edgeTriples[N, E](graph: Graph[N, E]): List[(Long, Long, E)] =
    edges(graph) map { e => (e.from, e.to, e.label) }

  // Returns the number of nodes
  def size[N, E](graph: Graph[N, E]) = graph.rep.size

  // Returns true if a node with a given id is present
  def hasNode[N, E](id: NodeId)(g: Graph[N, E]): Boolean = g.rep.contains(id)


  /*
   * Graph transformations
   */

  // Inserts a new node context into a graph.
  def insert[N, E](ctx: NodeContext[N, E])(graph: Graph[N, E]): Graph[N, E] =
    update(ctx.node.id)((_: Option[NodeContext[N, E]]) => Some(ctx))(graph)

  // Returns a new graph without a node associated to an id if it exists
  def remove[N, E](id: NodeId)(g: Graph[N, E]): Graph[N, E] = {
    val alwaysNone = (maybeCtx: Option[NodeContext[N, E]]) => None
    update(id)(alwaysNone)(g)
  }


  // Reverses the direction of every edge in a graph
  def reverseEdges[N, E](graph: Graph[N, E]): Graph[N, E] = {
    import scala.collection.generic._
    import scala.collection.immutable.LongMap.canBuildFrom
    val builder: CanBuildFrom[LongMap[NodeContext[N, E]],
      (Long, NodeContext[N, E]),
      LongMap[NodeContext[N, E]]] = canBuildFrom[NodeContext[N, E], NodeContext[N, E]]
    val graphRep: LongMap[NodeContext[N, E]] = graph.rep
    val newRep: LongMap[NodeContext[N, E]] = graphRep.map {
      case (id: Long, ctx: NodeContext[N, E]) =>
        (id, NodeContext(ctx.node, outgoing = ctx.incoming, incoming = ctx.outgoing))

    }(builder)
    Graph(rep = newRep)
  }

  // Union type for edge update (either remove or insert)
  private[graph] sealed trait EdgeUpdate

  private[graph] case class Insert[E](edge: E) extends EdgeUpdate

  private[graph] case class Remove[E](edge: E) extends EdgeUpdate

  // Represents a diff for updates corresponding to incoming and outgoing edges.
  private[graph] case class EdgeDiff[E](incoming: LongMap[EdgeUpdate], outgoing: LongMap[EdgeUpdate])

  private[graph] def emptyDiff[E] = EdgeDiff[E](LongMap.empty[EdgeUpdate], LongMap.empty[EdgeUpdate])

  /*
   * Returns the edge updates between two optional node contexts
   */
  private[graph] def getEdgeDiffs[N, E](before: Option[NodeContext[N, E]])
                                       (after: Option[NodeContext[N, E]]): EdgeDiff[E] = {

    def getUpdatesAcc(edgeUpdate: E => EdgeUpdate)
                     (acc: LongMap[EdgeUpdate], kv: (Long, E)): LongMap[EdgeUpdate] = {
      val (id, label) = kv

      def replaceUpdate(before: Option[EdgeUpdate]): Option[EdgeUpdate] =
        (before, edgeUpdate(label)) match {
          case (Some(Remove(oldLabel)), Insert(newLabel)) =>
            if (oldLabel == newLabel) None else Some(Insert(edge = newLabel))

          case (Some(Remove(_)), Remove(_)) =>
            throw new IllegalArgumentException(
              "Graph.getEdgeDiffs: Passed two removes for same edge. This is an illegal input.")

          case (Some(Insert(_)), _) =>
            throw new IllegalArgumentException(
              "Graph.getEdgeDiffs: An insert was passed before remove. This is an illegal input.")

          case (None, update) => Some(update)
        }

      val optUpdate: Option[EdgeUpdate] = acc.get(id) |> replaceUpdate

      optUpdate match {
        case None => acc
        case Some(eu) => acc.updated(id, eu)
      }
    }

    def collect(edgeUpdate: E => EdgeUpdate)
               (adj: Adjacency[E])
               (updates: LongMap[EdgeUpdate]): LongMap[EdgeUpdate] = {
      adj.foldLeft(updates)(getUpdatesAcc(edgeUpdate))
    }

    // Compute the edge difference between two optional node contexts
    (before, after) match {
      case (None, None) => emptyDiff[E]

      case (Some(rem), None) =>
        EdgeDiff(
          outgoing = LongMap.empty |> collect(Remove.apply)(rem.incoming),
          incoming = LongMap.empty |> collect(Remove.apply)(rem.outgoing)
        )

      case (None, Some(ins)) =>
        EdgeDiff(
          outgoing = LongMap.empty |> collect(Insert.apply)(ins.incoming),
          incoming = LongMap.empty |> collect(Insert.apply)(ins.outgoing)
        )

      case (Some(rem), Some(ins)) =>
        if (rem == ins) emptyDiff[E]
        else EdgeDiff(
          outgoing = LongMap.empty |> collect(Remove.apply)(rem.incoming) |> collect(Insert.apply)(ins.incoming),
          incoming = LongMap.empty |> collect(Remove.apply)(rem.outgoing) |> collect(Insert.apply)(ins.outgoing)
        )
    }
  }

  /*
   * Applies the edge updates contained in diff onto the graph
   * at a given nodeId. No operation is performend if no node corresponds to the id
   */
  private[graph] def applyEdgeDiff[N, E](id: NodeId)
                                        (diff: EdgeDiff[E])
                                        (rep: LongMap[NodeContext[N, E]]): LongMap[NodeContext[N, E]] = {
    def edgeUpdateToOption(u: EdgeUpdate): Option[E] = u match {
      case x: Insert[E] => Some(x.edge)
      case _: Remove[E] => None
    }

    def updateEdgeAdjacency(edgeFocus: Lens[NodeContext[N, E], Option[E]])
                           (updatedId: Long)
                           (edgeUpdate: EdgeUpdate)
                           (rep: LongMap[NodeContext[N, E]]): LongMap[NodeContext[N, E]] = {

      val edgeOpt: Option[E] = edgeUpdateToOption(edgeUpdate)
      val updateLabel = edgeFocus.update(x => edgeOpt)(_)
      updateLongMap(updatedId)((m: Option[NodeContext[N, E]]) => m.map(updateLabel))(rep)
    }

    val updateIncomingEdge: Long => EdgeUpdate => LongMap[NodeContext[N, E]] => LongMap[NodeContext[N, E]] =
      updateEdgeAdjacency(incoming[N, E] ~> lookup(id))

    val updateOutgoingEdge: Long => EdgeUpdate => LongMap[NodeContext[N, E]] => LongMap[NodeContext[N, E]] =
      updateEdgeAdjacency(outgoing[N, E] ~> lookup(id))

    rep |> foldl(updateIncomingEdge)(diff.incoming) |> foldl(updateOutgoingEdge)(diff.outgoing)
  }


  /*
   * Finds the node context of the given node id, and calls updater function
   * with the found Some(node) or None otherwise.
   * updater can return either a Some(updatedNode) or None to delete the node
   */
  private[graph] def update[N, E](nodeId: NodeId)
                                 (updater: Option[NodeContext[N, E]] => Option[NodeContext[N, E]])
                                 (graph: Graph[N, E]): Graph[N, E] = {

    def updateHelper(rep: LongMap[NodeContext[N, E]]): LongMap[NodeContext[N, E]] = {

      val oldNodeContext: Option[NodeContext[N, E]] = rep.get(nodeId)

      def filterInvalidEdges(ctx: NodeContext[N, E])
                            (edges: LongMap[E]): LongMap[E] = edges.filter {
        case (id, _) => ctx.node.id == id || rep.contains(id)
      }

      def cleanUpEdges(ctx: NodeContext[N, E]) = NodeContext(
        node = ctx.node,
        incoming = filterInvalidEdges(ctx)(ctx.incoming),
        outgoing = filterInvalidEdges(ctx)(ctx.outgoing)
      )

      val newNodeContext: Option[NodeContext[N, E]] =
        updater(oldNodeContext).map(cleanUpEdges)

      val diff = getEdgeDiffs(oldNodeContext)(newNodeContext)

      rep |> applyEdgeDiff(nodeId)(diff) |> updateLongMap(nodeId)(_ => newNodeContext)
    }

    Graph(rep = updateHelper(graph.rep))
  }

  /*
   * GRAPH BUILDERS
   */
  def fromNodesAndEdges[N, E](nodes: List[Node[N]])(edges: List[Edge[E]]): Graph[N, E] = {
    val initialRep: LongMap[NodeContext[N, E]] = {
      nodes.foldLeft(LongMap[NodeContext[N, E]]()) {
        case (acc: LongMap[NodeContext[N, E]], n: Node[N]) =>
          val ctx = NodeContext(n, LongMap[E](), LongMap[E]())
          acc.updated(n.id, ctx)
      }
    }

    def addEdge(edge: Edge[E])(rep: LongMap[NodeContext[N, E]]): LongMap[NodeContext[N, E]] = {
      def updateOutgoing(ctx: NodeContext[N, E]): NodeContext[N, E] = ctx.copy(
        outgoing = ctx.outgoing + ((edge.to, edge.label))
      )
      def updateIncoming(ctx: NodeContext[N, E]): NodeContext[N, E] = ctx.copy(
        incoming = ctx.incoming + ((edge.from, edge.label))
      )
      val f1 = (maybeCtx: Option[NodeContext[N, E]]) => maybeCtx.map(updateOutgoing)
      val f2 = (maybeCtx: Option[NodeContext[N, E]]) => maybeCtx.map(updateIncoming)
      rep |> updateLongMap(edge.from)(f1) |> updateLongMap(edge.to)(f2)
    }

    Graph(rep = edges.foldLeft(initialRep) {
      case (acc, e) => addEdge(e)(acc)
    })
  }


  def fromNodeLabelsAndEdges[N](labels: List[N])(links: List[(NodeId, NodeId)]): Graph[N, Unit] = {
    val nodes = {
      val nodesWithSize = labels.foldLeft((0L, List[Node[N]]())) {
        case ((id, ns), l) => (id + 1, Node(id, l) :: ns)
      }
      nodesWithSize._2
    }
    val edges = links map { case (from, to) => Edge(from, to, ()) }
    fromNodesAndEdges(nodes)(edges)
  }

  /*
   * GRAPH TRAVERSAL
   */

  /*
   * Type for node selection function
   * It takes a node context and returns a selection of neighboring nodes
   */
  type NeighborSelector[N, E] = NodeContext[N, E] => List[NodeId]

  // Returns a special selector that always returns the nodes along outgoing edge
  def selectOutgoingNodes[N, E]: NeighborSelector[N, E] =
    (ctx: NodeContext[N, E]) => ctx.outgoing.keys.toList

  /*
   * Type for a node visitor function
   * It takes a node context and an accumulator and returns a new accumulator
   */
  type SimpleNodeVisitor[N, E, A] = NodeContext[N, E] => A => A

  /*
   * Special node visitor function useful for depth-first search traversal
   * It takes a node context and an accumulator
   * and it returns a new accumulator along with a function
   */
  type DfsNodeVisitor[N, E, A] = NodeContext[N, E] => A => (A, A => A)

  /*
    Transforms a SimpleNodeVisitor into a DfsNodeVisitor
    that will be called upon node discovery.
   */
  def onDiscovery[N, E, A](visitor: SimpleNodeVisitor[N, E, A]): DfsNodeVisitor[N, E, A] =
    (ctx: NodeContext[N, E]) => (acc: A) => {
      val newAcc: A = visitor(ctx)(acc)
      (newAcc, identity[A])
    }

  /*
   * Transforms a SimpleNodeVisitor into a DfsNodeVisitor
   * that will be called upon node finish.
   */
  def onFinish[N, E, A](visitor: SimpleNodeVisitor[N, E, A]): DfsNodeVisitor[N, E, A] =
    (ctx: NodeContext[N, E]) => (acc: A) => (acc, visitor(ctx))

  /*
   * Depth-first traversal of a graph in no guaranteed order,
   * discovering nodes along outgoing edges.
   */
  def depthFirstSearch[N, E, A](visitor: DfsNodeVisitor[N, E, A])(acc: A)(graph: Graph[N, E]): A =
    dfsHelper[N, E, A](selectOutgoingNodes)(visitor)(nodeIds(graph))(acc)(graph)._1


  /*
   * Computes a depth-first forest corresponding to the graph spanned by
   * some seeds along outgoing edges
   * NodeContext[N,E] => A => (A, A => A)
   */
  def depthFirstForest[N, E](seeds: List[NodeId])(graph: Graph[N, E]): Forest[NodeContext[N, E]] = {

    def visit(ctx: NodeContext[N, E])(trees: Forest[NodeContext[N, E]]) = {
      val fn = (children: Forest[NodeContext[N, E]]) => {
        val t: Tree[NodeContext[N, E]] = Tree.construct(ctx)(children)
        t :: trees
      }
      (Nil, fn)
    }

    dfsHelper[N, E, Forest[NodeContext[N, E]]](selectOutgoingNodes)(visit)(seeds)(Nil)(graph)._1.reverse
  }


  /*
   * Helper function for depth first traversal
   */
  def dfsHelper[N, E, A](selector: NeighborSelector[N, E])
                        (visitor: DfsNodeVisitor[N, E, A])
                        (seeds: List[NodeId])
                        (acc: A)
                        (graph: Graph[N, E]): (A, Graph[N, E]) = {

    def run(seeds: List[NodeId])(acc: A)(graph: Graph[N, E]): TailRec[(A, Graph[N, E])] = seeds match {
      case Nil => done((acc, graph))
      case (next :: rest) => get(next)(graph) match {
        case None => tailcall(run(rest)(acc)(graph))
        case Some(ctx) =>
          val (accAfterDiscovery, finishNode) = visitor(ctx)(acc)
          val accBeforeFinishAndG: TailRec[(A, Graph[N,E])] =
            tailcall(run(selector(ctx))(accAfterDiscovery)(remove(next)(graph)))
          val fn: ((A, Graph[N,E])) => TailRec[(A, Graph[N,E])] = {
            case ((accBeforeFinish: A, g: Graph[N,E])) =>
              val accAfterFinish = finishNode(accBeforeFinish)
              run(rest)(accAfterFinish)(g)
          }
          accBeforeFinishAndG.flatMap(fn)
      }
    }

    run(seeds)(acc)(graph).result
  }


  /*
   * Decomposes a graph into its strongly connected components.
   * This implementation is based on Kosaraju-Sharir algorithm. For further details, see:
   * [1] Micha Sharir. A strong connectivity algorithm and its applications to data flow analysis.
   *   Computers and Mathematics with Applications 7(1):67–72, 1981.
   * [2] Cormen et al. Introduction to Algorithms, 3rd edition. The MIT Press, 2009.
   * [3] https://en.wikipedia.org/wiki/Kosaraju%27s_algorithm
   */
  def stronglyConnectedComponents[N, E](graph: Graph[N, E]): List[Graph[N, E]] = {
    val now = System.nanoTime

    val seeds: List[NodeId] = {
      val simpleVisitor: NodeContext[N, E] => List[NodeId] => List[NodeId] =
        (ctx) => (acc) => ctx.node.id :: acc
      depthFirstSearch(onFinish(simpleVisitor))(Nil)(graph)
    }
    val forest = depthFirstForest(seeds)(reverseEdges(graph))
    val folder: (NodeContext[N, E], Graph[N, E]) => Graph[N, E] = insert(_)(_)
    val insertHelper: List[NodeContext[N, E]] => Graph[N, E] = _.foldRight(empty[N, E])(folder)
    val components: List[Graph[N, E]] = forest map { case (tree: Tree[NodeContext[N, E]]) =>
      val tls: List[NodeContext[N, E]] = Tree.preOrderList(tree)
      val g = insertHelper(tls)
      reverseEdges(g)
    }
    val micros = (System.nanoTime - now) / 1000
    println(s"Computing connected components - time: $micros microseconds")
    components
  }

  /*
   * Helper functions
   */

  /*
   * Updating a LongMap (used for Adjacency updates) in the following way:
   * - when the key exists, it will
   *     - update the value if f returns Some[V]
   *     - remove the (key, value) pair if f returns None
   *
   * - when the key does not yet exist, it will
   *     - inserts a new (key, value) pair if f returns Some[V]
   *     - not modify anything if f returns None
   *
   */
  def updateLongMap[V](key: Long)
                      (f: Option[V] => Option[V])
                      (dict: LongMap[V]): LongMap[V] = {
    val fVal = dict.get(key) |> f

    (dict.contains(key), fVal) match {
      case (_, Some(v)) => dict + ((key, v))
      case (true, None) => dict - key
      case (false, None) => dict
    }
  }

  /*
   * A flipped foldLeft for LongMap for having a point-free notation
   */
  def foldl[A, B](fn: Long => B => A => A)
                 (dict: LongMap[B])
                 (acc: A): A = {
    def f(a: A, kv: (Long, B)): A = fn(kv._1)(kv._2)(a)
    dict.foldLeft(acc)(f)
  }

  /*
   * Lenses focusing on an incoming or outgoing edge(s)
   */
  def incoming[N, E]: Lens[NodeContext[N, E], LongMap[E]] = Lens.focus(
    get = (d: NodeContext[N, E]) => d.incoming,
    update = (f: LongMap[E] => LongMap[E]) => (d: NodeContext[N, E]) => d.copy(
      incoming = f(d.incoming))
  )

  def outgoing[N, E]: Lens[NodeContext[N, E], LongMap[E]] = Lens.focus(
    get = (d: NodeContext[N, E]) => d.outgoing,
    update = (f: LongMap[E] => LongMap[E]) => (d: NodeContext[N, E]) => d.copy(
      outgoing = f(d.outgoing))
  )

  def lookup[E](nodeId: NodeId): Lens[LongMap[E], Option[E]] = Lens(
    (d: LongMap[E]) => d.get(nodeId),
    (fn: Option[E] => Option[E]) => (d: LongMap[E]) => updateLongMap(nodeId)(fn)(d)
  )


  /*
   * Higher-order helpers
   */
  // Implicit for obtaining the forward pipe operator in Scala
  // so we can write f(x) as: x |> f and g(f(x)) as: x |> f |> g
  implicit class PipeOp[A](a: A) {
    def |>[B](f: A => B): B = f(a)
  }

}
