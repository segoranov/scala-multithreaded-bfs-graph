package graph

import graph.Graph.{AdjMatrix, Row}
import org.scalatest.{FlatSpec, Matchers}

class GraphTest extends FlatSpec with Matchers {

  val testGraph = Graph(AdjMatrix(  Row(0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0),
                                    Row(1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0),
                                    Row(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                                    Row(1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0),
                                    Row(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
                                    Row(1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1),
                                    Row(0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0),
                                    Row(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0),
                                    Row(0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0),
                                    Row(0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0),
                                    Row(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0)
                                  ))

  "all vertices" should "be from 0 to 10" in {
    testGraph.getVertices shouldBe List.range(0, 11)
    testGraph.getVertices.size shouldBe 11
  }

  "vertices" should "be in graph" in {
    List.range(0, 11).foreach(vertex => testGraph.hasVertex(vertex) shouldBe true)
  }

  "vertices" should "not be in graph" in {
    List.range(-10, 0).foreach(vertex => testGraph.hasVertex(vertex) shouldBe false)
    List.range(11, 30).foreach(vertex => testGraph.hasVertex(vertex) shouldBe false)
  }

  "edges" should "either exists or not exist" in {

    testGraph.isEdge(0,1) shouldBe true
    testGraph.isEdge(0,0) shouldBe false

    testGraph.getVertices.foreach(vertex => {
      if (vertex == 5)
        testGraph.isEdge(10, vertex) shouldBe true
      else
        testGraph.isEdge(10, vertex) shouldBe false
    })
  }
}
