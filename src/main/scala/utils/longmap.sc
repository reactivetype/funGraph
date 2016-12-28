import graph.Graph
import graph.Graph.{Node, NodeContext, updateLongMap}
import utils.Lens

import scala.collection.immutable.LongMap

val d = LongMap(
  (1L, "one"), (2L, "two"), (3L, "three")
)

val d2 = updateLongMap[String](3L)(x => Some("four"))(d)

val d3 = updateLongMap[String](3L)(x => None)(d)

d + ((3L, 100))

d - 3L

d - 99L


//d.updateWith(3L, "four", (curr, s) => curr)
//d.updateWith(3L, "four", (curr, s) => s)
//d.updateWith(4L, "four", (curr, s) => curr)

val ctx: NodeContext[String, Int] = NodeContext(Node(3L, "C"), LongMap(2L -> 77), LongMap(4L -> 66))
val focusOnIncoming = Graph.incoming[String, Int] ~> Graph.lookup(2L)
val newCtx = ctx.copy(
  incoming = LongMap.empty
)

val dict: LongMap[Int] = ctx.incoming
updateLongMap[Int](2L)((m: Option[Int]) => None)(dict)

Lens.get(focusOnIncoming)(ctx)
Lens.set(focusOnIncoming)(None)(ctx)