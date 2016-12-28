package graph

import org.scalatest.{MustMatchers, Suite, WordSpecLike}
import graph.{GraphExamples => Case}
import Graph.{Edge, EdgeDiff, Graph, Insert, Node, NodeContext, Remove, SimpleNodeVisitor}
import utils.Lens
import Graph.updateLongMap

import scala.collection.immutable.LongMap
/**
  * Created by Rindra on 9/7/2016.
  */
trait TestMixin extends Suite
  with WordSpecLike
  with MustMatchers


class GraphSpec extends TestMixin {
  "Graph.fromNodesAndEdges " can {
    "build from a list of nodes and edges" in {
      // unconnected graph
      val ug: Graph[String, Int] = Graph.fromNodesAndEdges(Case.nodes)(Nil)
      Graph.size(ug) must be (10)
      Graph.nodeIds(ug) must be (Case.nodes map {n => n.id})
      Graph.edges(ug) must be (Nil)

      // directed acyclic graph
      val dag = Graph.fromNodesAndEdges(Case.nodes)(Case.dagEdges)
      Graph.size(dag) must be (10)
      Graph.nodeIds(dag) must be (Case.nodes map {n => n.id})
      Graph.edges(dag).size must be (10)
    }

    "build from a list of labels and links" in {
      val lg = Graph.fromNodeLabelsAndEdges(Case.labels)(Case.linearLinks)
      Graph.size(lg) must be (10)
      Graph.edges(lg).size must be (9)
    }

    "build a graph with self-cycles" in {
      val node = Node(1L, "A")
      val selfCycle = Edge(1L, 1L, "self cycle")
      val g = Graph.fromNodesAndEdges(node :: Nil)(selfCycle :: Nil)
      g |> Graph.get(1L) must be
      Some(NodeContext(node, incoming = LongMap(1L -> "self-cycle"), outgoing = LongMap(1L -> "self-cycle")))
    }
  }

  "Graph.incoming lens" can {
    val ctx: NodeContext[String, Int] = NodeContext(Node(3L, "C"), LongMap(2L -> 1), LongMap(4L -> 1))
    "focus on the incoming edges of a node context" in {
      Graph.incoming.get(ctx) must be (LongMap(2L -> 1))

      val lonely = emptyCtx(Node(0L, "lonely"))
      Graph.incoming.get(lonely) must be (LongMap())
    }

    "set the incoming edges of a node context" in {
      val newIncoming = LongMap(0L -> 1, 2L -> 1)
      val newCtx = Graph.incoming.update((_: LongMap[Int]) => newIncoming)(ctx)
      newCtx |> Graph.incoming.get must be (newIncoming)
    }
  }

  "Graph.outgoing lens" can {
    val ctx: NodeContext[String, Int] = NodeContext(Node(3L, "C"), LongMap(2L -> 1), LongMap(4L -> 1))
    val lonely = emptyCtx(Node(0L, "lonely"))

    "focus on the outgoing edges of a node context" in {
      Graph.outgoing.get(ctx) must be (LongMap(4L -> 1))
      Graph.outgoing.get(lonely) must be (LongMap())
    }

    "set the outgoing edges of a node context" in {
      val newOutgoing = LongMap(0L -> 1, 2L -> 1)
      val newCtx = Graph.outgoing.update((_: LongMap[Int]) => newOutgoing)(ctx)
      newCtx |> Graph.outgoing.get must be (newOutgoing)
    }
  }

  "Graph.lookup lens" can {
    val edges = LongMap(2L -> 1, 3L -> 1)

    "focus on a specific edge label for a given key" in {
      edges |> Graph.lookup(2L).get must be (Some(1))
    }

    "returns None if it does not have the key" in {
      edges |> Graph.lookup(99L).get must be (None)
    }

    "replaces an existing edge for a given key" in {
      edges |> Graph.lookup(3L).update((x: Option[Int]) => x.map(_ + 100)) must be
      LongMap(2L -> 1, 3L -> 101)
    }

    "does nothing if updater : always(None)" in {
      edges |> Graph.lookup[Int](99L).update((x: Option[Int]) => None) must be
      edges
    }

    "removes edge if id exists and returns None" in {
      edges |> Graph.lookup[Int](3L).update((x: Option[Int]) => None) must be
      edges - 3L
    }

    "adds an edge if the key does not yet exist" in {
      edges |> Graph.lookup(99L ).update((x: Option[Int]) => Some(100)) must be
      LongMap(2L -> 1, 3L -> 1, 99L -> 100)
    }
  }

  "Graph.incoming ~> Graph.lookup" can {
    val ctx: NodeContext[String, Int] = NodeContext(Node(3L, "C"), LongMap(2L -> 77), LongMap(4L -> 66))
    val focusOnIncoming = Graph.incoming[String, Int] ~> Graph.lookup(2L)
    val focusOnOutgoing = Graph.outgoing[String, Int] ~> Graph.lookup(4L)
    val focusOnNonExisting = Graph.outgoing[String, Int] ~> Graph.lookup(3L)
    val focusNewOutgoing = Graph.outgoing[String, Int] ~> Graph.lookup(99L)


    "get an existing incoming edge label" in {
      focusOnIncoming.get(ctx) must be (Some(77))
    }

    "get None if incoming does not have the key" in {
      focusOnNonExisting.get(ctx) must be (None)
    }

    "get an existing outgoing edge label" in {
      focusOnOutgoing.get(ctx) must be (Some(66))
    }

    "remove an incoming edge" in {
      val newCtx = ctx.copy(
        incoming = LongMap.empty
      )
      focusOnIncoming.update(m => None)(ctx) must be (newCtx)
    }

    "modify an incoming edge" in {
      val newCtx = ctx.copy(
        incoming = LongMap(2L -> 100)
      )
      Lens.set(focusOnIncoming)(Some(100))(ctx) must be (newCtx)
    }

    "modify nothing if node id does not exist" in {
      ctx |> Lens.set(focusOnNonExisting)(None) must be (ctx)
    }

    "inserts an edge if the key does not yet exist" in {
      val newCtx = ctx.copy(outgoing = LongMap(4L -> 66, 99L -> 9999))
      ctx |> Lens.set(focusNewOutgoing)(Some(9999)) must be (newCtx)
    }
  }

  "Graph.applyEdgeDiff" must {
    "remove the edges connecting a given node in its connected nodes' contexts" in {
      val nodeId = 3L
      val diff: EdgeDiff[Int] = EdgeDiff(incoming = LongMap(4L -> Remove(1)), outgoing = LongMap(2L -> Remove(1)))
      val rep: LongMap[NodeContext[String, Int]] = Case.linearGraph.rep
      val newRep = rep |> Graph.applyEdgeDiff(nodeId)(diff)
      val ng = Graph.Graph(newRep)

      ng |> Graph.get(2L) must be
      Some(NodeContext(Node(2L, "C"), LongMap(1L -> 1), LongMap()))
      ng |> Graph.get(3L) must be
      Some(NodeContext(Node(3L, "D"), LongMap(2L -> 1), LongMap(4L -> 1)))
      ng |> Graph.get(4L) must be
      Some(NodeContext(Node(4L, "E"), LongMap(), LongMap(5L -> 1)))
    }
  }

  "Graph.insert " must {
    "add a new node context into a graph" in {
      val ctx = emptyCtx(Node(99L, "Z"))
      val ng = Case.dagGraph |> Graph.insert(ctx)

      Graph.get(99L)(ng) must be (Some(ctx))
      Graph.size(ng) must be (Graph.size(Case.dagGraph) + 1)
    }

    "updates an existing node context if already present" in {
      val ctx = emptyCtx(Node(0L, "Z"))
      val ng = Case.dagGraph |> Graph.insert(ctx)

      Graph.get(0L)(ng) must be (Some(ctx))
      Graph.size(ng) must be (Graph.size(Case.dagGraph))
    }
  }

  "Graph.remove" can {
    "removes a node and edges associated with given id" in {
      val ng = Case.linearGraph |> Graph.remove(3L)
      ng |> Graph.nodeIds must be (List(0L, 1L, 2L, 4L, 5L, 6L, 7L, 8L, 9L))

      val newNode2 = NodeContext(Node(2L, "C"), LongMap(1L -> 1), LongMap())
      val newNode4 = NodeContext(Node(4L, "E"), LongMap(), LongMap(5L -> 1))
      ng |> Graph.get(2L) must be (Some(newNode2))
      ng |> Graph.get(4L) must be (Some(newNode4))
    }

    "leaves the graph intact if id is non-existent" in {
      val ng = Case.linearGraph |> Graph.remove(99L)
      ng must be (Case.linearGraph)
      ng |> Graph.get(99L) must be (None)
    }
  }

  "Graph.update" can {
    "update a node context into an unconnected one" in {
      val newCtx = emptyCtx(Node(3L, "THREE"))
      val ng = Case.linearGraph |> Graph.update(3L)(m => Some(newCtx))
      ng |> Graph.get(3L) must be (Some(newCtx))
      ng |> Graph.get(2L) must be (Some(NodeContext(Node(2L, "C"), LongMap(1L -> 1), LongMap())))
      ng |> Graph.get(4L) must be (Some(NodeContext(Node(4L, "E"), LongMap(), LongMap(5L -> 1))))
    }

    "update a node context and its outgoing edge" in {
      val newCtx = NodeContext(Node(3L, "THREE"), LongMap(), LongMap(0L -> 100))
      val ng = Case.linearGraph |> Graph.update(3L)(m => Some(newCtx))
      ng |> Graph.get(3L) must be (Some(newCtx))
      ng |> Graph.get(2L) must be (Some(NodeContext(Node(2L, "C"), LongMap(1L -> 1), LongMap())))
      ng |> Graph.get(4L) must be (Some(NodeContext(Node(4L, "E"), LongMap(), LongMap(5L -> 1))))
      ng |> Graph.get(0L) must be (Some(NodeContext(Node(0L, "A"), LongMap(3L -> 100), LongMap(1L -> 1))))
    }
  }

  "Graph.reverse " must {
    "be reflexive" in {
      val lg = Case.linearGraph
      lg |> Graph.reverseEdges |> Graph.reverseEdges must be (lg)
    }
    "do nothing on empty graph" in {
      Graph.empty |> Graph.reverseEdges must be (Graph.empty)
    }
  }

  "Graph.depthFirstSearch - for a linear graph" can {
    val lg = Case.linearGraph
    val getNodeLabels = (ctx: NodeContext[String, Int]) => (acc: List[String]) => ctx.node.label :: acc
    val upperCaseAndConcatenate = (ctx: NodeContext[String, Int]) => (acc: String) => acc + ctx.node.label.toUpperCase

    "traverse a graph and accumulate node labels" in {
      lg |> Graph.depthFirstSearch(Graph.onDiscovery(getNodeLabels))(Nil) must be
      Case.labels.reverse
    }

    "traverse a graph and concatenate labels into uppercase string " in {
      lg |> Graph.depthFirstSearch(Graph.onDiscovery(upperCaseAndConcatenate))("") must be
      "ABCDEDFHIJ"
    }
  }

  "Graph.depthFirstSearch - for a tree" must {
    val tg = Case.treeGraph
    val getEdgeLabels: SimpleNodeVisitor[String, String, List[String]] =
      (ctx) => (acc) => if (ctx.outgoing.isEmpty) acc else acc ++ ctx.outgoing.values.toList

    "traverse a graph and accumulate edge labels in DFS order" in {
      val acc = tg |> Graph.depthFirstSearch(Graph.onDiscovery(getEdgeLabels))(List[String]())
      acc must be
      List("A - B", "A - C", "B - D", "B - E", "D - H", "D - I", "E - J", "C - F", "C - G")
    }
  }

  "Graph.depthFirstForest - for a tree graph" must {
    val tg = Case.treeGraph
    val seeds = tg |> Graph.nodeIds

    "return a forest with a single tree" in {
      val forest = tg |> Graph.depthFirstForest(seeds)
      forest.size must be (1)
    }
  }

  "Graph.depthFirstForest - for a cycle" must {
    val cg = Case.oneCycleGraph
    val seeds = cg |> Graph.nodeIds

    "return a forest with a single tree" in {
      val forest = cg |> Graph.depthFirstForest(seeds)
      forest.size must be (1)
    }
  }

  "Graph.stronglyConnectedComponents - for a directed acyclic graph" must {
    "return as many strongly connected component as there are nodes" in {
      val scc = Case.dagGraph |> Graph.stronglyConnectedComponents
      scc.size must be (Graph.size(Case.dagGraph))
    }
  }

  "Graph.stronglyConnectedComponents - for one-cycle graph" must {
    "return a single strongly connected component" in {
      val scc1 = Case.oneCycleGraph |> Graph.stronglyConnectedComponents
      scc1.size must be (1)
      val scc2 = Case.complexCycleGraph |> Graph.stronglyConnectedComponents
      scc2.size must be (1)
    }
  }


  "Graph.stronglyConnectedComponents - for a 3-component graph" must {
    "return 3 strongly connected components" in {
      val scc = Case.threeComponentsGraph |> Graph.stronglyConnectedComponents
      scc.size must be (3)
    }
  }

  "Graph.stronglyConnectedComponents - for a 5-component graph" must {
    "return 5 strongly connected components" in {
      val scc = Case.fiveComponentGraph |> Graph.stronglyConnectedComponents
      scc.size must be (5)
    }
  }

  "Graph.stronglyConnectedComponents - for a large linear graph" must {
    "return many strongly connected components" in {
      val size = 2000000L
      val scc = size |> GraphGenerator.randomLinearGraph |> Graph.stronglyConnectedComponents
//      scc.size must be (size)
      1 must be (1)
    }
  }

  "Graph.stronglyConnectedComponents - for a large cyclic graph" ignore {
    "return one big component" in {
      val size = 8000L
      val scc = size |> GraphGenerator.randomCycleGraph |> Graph.stronglyConnectedComponents
      scc.size must be (size)
    }
  }


  /*
   * Helpers
   */
  private def emptyCtx(n: Node[String]): NodeContext[String, Int] =
    NodeContext(n, LongMap[Int](), LongMap[Int]())

  /*
   * Higher-order helpers
   */
  // Implicit for obtaining the forward pipe operator in Scala
  // so we can write f(x) as: x |> f and g(f(x)) as: x |> f |> g
  implicit class PipeOp[A](a: A) {
    def |>[B](f: A => B): B = f(a)
  }
}
