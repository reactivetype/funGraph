# funGraph
funGraph is functional graph processing library in Scala. 

# Graph representation
The Graph representation is based on inductive graph that was proposed by Martin Erwig.

  * A graph is internally represented by a hash table of `NodeContext`
  * A `NodeContext` represents a node with its incoming and outgoing edges
  * The collection of edges or `Adjacency`is stored in a Patricia trie for efficient edge removal and insertion 


# Graph operations
The following graph operations are currently implemented:
- Graph builders: builds a graph from a list of edges and/or nodes
- Query functions: 
   * get the node context associated to a node id
   * returns a list of all node ids or edges
- Transformations:
   * Insert or remove a new node context into a graph
   * Reverses the direction of every edge in the graph
- Traversal functions: 
   * Depth-first traversal 
   * Depth-first forest: computes a list of trees spanned by some specified seeds along outgoing edges
   * Strongly connected components: decompose a graph into componends based on Kosaraju-Sharir algorithm

[1] Martin Erwig. 2001. _Inductive graphs and functional graph algorithms._ J. Funct. Program. 11, 5 (September 2001), 467-492.

[2] Micha Sharir. _A strong connectivity algorithm and its applications to data flow analysis._
