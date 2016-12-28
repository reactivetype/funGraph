package graph

import Graph.NodeContext
import GraphGenerator._

/**
  * Created by Rindra on 9/13/2016.
  */
class GraphGeneratorSpec extends TestMixin {

  "randomLinearGraph " can {
    "build a random linear graph of a given size" in {
      val g = randomLinearGraph(4)

      Graph.size(g) must be (4)

      g.rep.foreach { case (id: Long, ctx: NodeContext[String, Int]) =>
        ctx.incoming.size must be <= 1
        ctx.outgoing.size must be <= 1
        ctx.incoming.size + ctx.outgoing.size must be > 0
      }
    }

  }

  "randomCycleGraph " can {
    "build a random graph with one cycle of a given size" in {
      val c = randomCycleGraph(4)

      Graph.size(c) must be (4)
      Graph.edges(c).size must be (Graph.size(c))

      c.rep.foreach { case (id: Long, ctx: NodeContext[String, Int]) =>
        ctx.incoming.size must be (1)
        ctx.outgoing.size must be (1)
      }
    }

  }

  /*
 * Higher-order helpers
 */
  // Implicit for obtaining the forward pipe operator in Scala
  // so we can write f(x) as: x |> f and g(f(x)) as: x |> f |> g
  implicit class PipeOp[A](a: A) {
    def |>[B](f: A => B): B = f(a)
  }

}

