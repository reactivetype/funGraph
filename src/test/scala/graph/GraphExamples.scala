package graph


/**
  * Created by Rindra on 9/7/2016.
  */
object GraphExamples {

  import Graph._

  /*
   * Empty graph
   */
  val emptyGraph = empty[String, Int]

  /*
   * A list of labels and edges
   */
  val labels: List[String] = List("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")
  val nodes: List[Node[String]] = labels.zipWithIndex map { case (l, id) => Node(id.toLong, l) }

  /*
   * Linear graph (linked list)
   */
  val linearEdges = List(
    Edge(0L, 1L, 1), Edge(1L, 2L, 1), Edge(2L, 3L, 1),
    Edge(3L, 4L, 1), Edge(4L, 5L, 1), Edge(5L, 6L, 1),
    Edge(6L, 7L, 1), Edge(7L, 8L, 1), Edge(8L, 9L, 1)
  )
  val linearLinks = linearEdges map { e => (e.from, e.to) }

  /*
   * A graph without edges
   */
  val noEdges = fromNodeLabelsAndEdges(labels)(Nil)

  /*
   * A graph with linear edges
   */
  val linearGraph = fromNodesAndEdges(nodes)(linearEdges)

  /*
   * A tree
   */
  val treeEdges = List(
    Edge(0L, 1L, "A - B"), Edge(0L, 2L, "A - C"), Edge(1L, 3L, "B - D"),
    Edge(1L, 4L, "B - E"), Edge(2L, 5L, "C - F"), Edge(2L, 6L, "C - G"),
    Edge(3L, 7L, "D - H"), Edge(3L, 8L, "D - I"), Edge(4L, 9L, "E - J")
  )
  val treeGraph: Graph[String, String] = fromNodesAndEdges(nodes)(treeEdges)

  /*
   * A Directed Acyclic Graph
   */
  val dagEdges = List(
    Edge(0L, 1L, 1), Edge(0L, 2L, 1),
    Edge(1L, 3L, 1), Edge(1L, 4L, 1), Edge(2L, 5L, 1), Edge(2L, 6L, 1),
    Edge(3L, 7L, 1), Edge(3L, 8L, 1), Edge(4L, 8L, 1), Edge(4L, 9L, 1)
  )
  val dagGraph = fromNodesAndEdges(nodes)(dagEdges)

  /*
   * A cyclic graph with one single simple cycle
   */
  val oneCycle = Edge(9L, 0L, 1) :: linearEdges
  val oneCycleGraph = fromNodesAndEdges(nodes)(oneCycle)

  /*
   * A cyclic graph with one complex cycle (two connected simple cycles)
   */
  val complexCycle = Edge(9L, 4L, 1) :: Edge(4L, 0L, 1) :: linearEdges
  val complexCycleGraph = fromNodesAndEdges(nodes)(complexCycle)


  /*
   * A graph with 3 strongly connected components
   */
  val threeComponentEdges = List(
    /// first component: nodes 0 to 3
    Edge(0L, 1L, 1), Edge(1L, 2L, 1), Edge(2L, 3L, 1), Edge(3L, 0L, 1),

    // second component: node 4

    // third component: nodes 5 to 9
    Edge(5L, 6L, 1), Edge(6L, 7L, 1), Edge(7L, 8L, 1), Edge(8L, 9L, 1), Edge(9L, 7L, 1), Edge(7L, 5L, 1)
  )
  val threeComponentsGraph = fromNodesAndEdges(nodes)(threeComponentEdges)

  /*
   * A graph with 5 strongly connected components
   */
  val fiveComponentEdges = Edge(9L, 4L, 1) :: Edge(0L, 4L, 1) :: linearEdges
  val fiveComponentGraph = fromNodesAndEdges(nodes)(fiveComponentEdges)

}
