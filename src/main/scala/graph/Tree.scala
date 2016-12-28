package graph


object Tree {


  /*
   * Represents an n-ary tree of nodes labeled by
   * some values of a generic type L
   */
  case class Tree[L](size: Int, root: Option[(L, List[Tree[L]])])


  /*
   * Represents a forest, an alias for a list of trees
   */
  type Forest[L] = List[Tree[L]]

  // Returns an empty tree
  def empty[L]: Tree[L] = Tree(0, None)

  // Returns true if the tree is empty
  def isEmpty[L](tree: Tree[L]): Boolean = tree.size == 0


  /*
   * Constructs a tree given a root node a list (potentially empty) of subtrees children
   */
  def construct[L](label: L)(children: List[Tree[L]]): Tree[L] = {
    val nonEmptyChildren = children.filterNot(isEmpty)
    val sz = children.foldLeft(1){ (cnt: Int, t: Tree[L]) =>
      cnt + t.size
    }
    Tree(sz, Some(label, nonEmptyChildren))
  }

  // Constructs a tree with a single leaf node
  def leaf[L](label: L): Tree[L] = construct(label)(Nil)


  /*
   * Depth-first pre-order tree traversal
   * https://en.wikipedia.org/wiki/Tree_traversal
   */

  // Helper traversal function -- OK having to annotate this one in Scala is crazy
  def listForTraversal[L](traversal: (L => Forest[L] => (List[L] => List[L]) => List[L] => List[L]) => (List[L] => List[L]) => Tree[L] => List[L] => List[L])
                         (tree: Tree[L]): List[L] = {

    def fn(label: L)(children: Forest[L])(rest: List[L] => List[L]): List[L] => List[L]  =
      (ls: List[L]) => rest(label :: ls)

    traversal(fn)(identity)(tree)(Nil)
  }

  /*
   * Pre-order traversal
   */
  def preOrder[L, A](visit: L => Forest[L] => A => A)(acc: A)(tree: Tree[L]): A
  = {
    val folder = (acc: A, tree: Tree[L]) => preOrder(visit)(acc)(tree)
    tree.root match {
      case None => acc
      case Some((label, children)) => children.foldLeft(visit(label)(children)(acc))(folder)
    }
  }

  /*
   * Takes a tree and returns the list of labels traversed with pre-order DFS traversal
   */
  def preOrderList[L]: Tree[L] => List[L] =
    listForTraversal[L](preOrder)

}