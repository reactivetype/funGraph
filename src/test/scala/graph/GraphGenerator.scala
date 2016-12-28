package graph

import Graph._
import scala.util.Random
import scala.collection.immutable.LongMap

/**
  * Created by giocode on 2016-09-14.
  */
object GraphGenerator {

  def randomNode: Node[String] = {
    val id = Random.nextLong()
    Node(id, id.toString)
  }

  /*
   * Creates a list of random nodes of type Node[String]
   */
  def randomNodes(n: Long) = {
    def randomNodesAcc(acc: List[Node[String]], rem: Long): List[Node[String]] = rem match {
      case 0 => acc
      case i => randomNodesAcc(randomNode :: acc, i-1)
    }
    randomNodesAcc(Nil, n)
  }

  def emptyCtx[N,E](node: Node[N]) = NodeContext(node, LongMap[E](), LongMap[E]())

  /*
   * Creates a list of edges of type Edge[Int] that linearly connect a given list of nodes
   */
  def linearEdges[N](nodes: List[Node[N]]): List[Edge[Int]] = {
    def linearEdgesAcc(acc: List[Edge[Int]], rem: List[Node[N]]): List[Edge[Int]] = (acc, rem) match {
      case (Nil, ns) if ns.size < 2 => Nil    // two few nodes to create edges

      case (Nil, n1 :: n2 :: rest) =>
        linearEdgesAcc(List(Edge(n1.id, n2.id, 0)), rest)  // else create first edge

      case (edges, Nil ) => edges   // no more nodes, return accumulated edges

      case (edges@(e :: _), n :: ns) =>   // process next node and link to last create edge's destination node
        val newEdge = Edge(e.to, n.id, 0)
        linearEdgesAcc(newEdge :: edges, ns)
    }

    linearEdgesAcc(Nil, nodes)
  }

  /*
   * Generates a random linear graph of a given size and type Graph[String, Int]
   */
  def randomLinearGraph(size: Long): Graph[String, Int] = {
    require (size > 2)
    val now = System.nanoTime
    val nodes = randomNodes(size)
    val edges = linearEdges(nodes)
    val result = Graph.fromNodesAndEdges(nodes)(edges)
    val micros = (System.nanoTime - now) / 1000
    println(s"Constructing random graph => size: $size - time: $micros microseconds")
    result
  }

  /*
   * Generates a random one-cycle graph of a given size and type Graph[String, Int]
   */
  def randomCycleGraph(size: Long): Graph[String, Int] = {
    require (size > 2)
    val nodes = Node(0L, "AnchorNode") :: randomNodes(size-1)
    val les = linearEdges(nodes)
    val lastEdge = les.head
    val edges = Edge(lastEdge.to, 0L, 0) :: les
    Graph.fromNodesAndEdges(nodes)(edges)
  }

}
