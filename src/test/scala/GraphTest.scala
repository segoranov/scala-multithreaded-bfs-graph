package graph

import org.scalatest.{FlatSpec, Matchers}

class GraphTest extends FlatSpec with Matchers {
  "test" should "be always true" in {
    val g = Graph(5)
    g.test shouldBe true
  }

  it should "be always true in" in {
    val g = Graph(5)
    g.test shouldBe true
  }

}
