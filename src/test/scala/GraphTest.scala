package graph

import graph.Graph.{AdjMatrix, Row}
import graph.Timer.{TimeElapsedInMilliseconds, time}
import org.scalatest.{FlatSpec, Matchers}
import java.io.{File, PrintWriter}

class GraphTest extends FlatSpec with Matchers {

  val testGraph = Graph(AdjMatrix(
    Row(0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0),
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

  val testGraphManyVertices = Graph.withRandomEdges(numberOfVertices = 200)

  "all vertices" should "be from 0 to 10" in {
    testGraph.getVertices shouldBe List.range(0, 11).toSet
    testGraph.getNumVertices shouldBe 11
  }

  "vertices" should "be in graph" in {
    List.range(0, 11).foreach(vertex => testGraph.hasVertex(vertex) shouldBe true)
  }

  "vertices" should "not be in graph" in {
    List.range(-10, 0).foreach(vertex => testGraph.hasVertex(vertex) shouldBe false)
    List.range(11, 30).foreach(vertex => testGraph.hasVertex(vertex) shouldBe false)
  }

  "edges" should "either exists or not exist" in {
    testGraph.isEdge(0, 1) shouldBe Right(true)
    testGraph.isEdge(0, 0) shouldBe Right(false)

    testGraph.getVertices.foreach(vertex => {
      if (vertex == 5)
        testGraph.isEdge(10, vertex) shouldBe Right(true)
      else
        testGraph.isEdge(10, vertex) shouldBe Right(false)
    })
  }

  "graph" should "be incorrect and throw exception" in {
    assertThrows[IllegalArgumentException] {
      Graph(AdjMatrix(Row(0, 0, 4), Row(0, 0, 0), Row(0, 0, 0)))
      Graph(AdjMatrix(Row(0, 0, 1), Row(0, 0, 0)))
    }
  }

  "vertex 3" should "have vertices 0, 8 and 9 as neighbours" in {
    testGraph.getNeighbours(3) shouldBe Right(Set(0, 8, 9))
  }

  "vertex 1" should "have vertices 0 and 6 as neighbours" in {
    testGraph.getNeighbours(1) shouldBe Right(Set(0, 6))
  }

  "bfs traversal starting from all vertices" should "generate paths which contain all vertices" in {
    testGraph.getVertices.foreach(vertex => {
      testGraph.bfsTraversalFrom(vertex).toSet shouldBe testGraph.getVertices
    })
  }

  "bfs traversal starting from all vertices" should
    "be faster when the threads (tasks) are more" in {
    type NumberOfTasks = Int

    val mapNumberOfThreadsToTimeElapsed =
      List.range(1, Runtime.getRuntime.availableProcessors)
        .foldLeft[Map[NumberOfTasks, TimeElapsedInMilliseconds]](Map.empty)((acc, numberOfTasks) => {

        val millisecondsElapsed = testGraphManyVertices.bfsTraversalStartingFromAllVertices(numberOfTasks)
        acc + (numberOfTasks -> millisecondsElapsed)
      })

    mapNumberOfThreadsToTimeElapsed.foreach(pair => {
      mapNumberOfThreadsToTimeElapsed
        .filter(other => other._1 < pair._1)
        .foreach(other => other._2 should be > pair._2)
    })
  }

  "reading graph from file" should "be correct" in {

    var fileContent =
      """11
0 1 1 1 1 1 0 0 0 0 0
1 0 0 0 0 0 1 0 0 0 0
1 0 0 0 0 0 0 0 0 0 0
1 0 0 0 0 0 0 0 1 1 0
1 0 0 0 0 1 0 0 0 0 0
1 0 0 0 1 0 0 0 0 0 1
0 1 0 0 0 0 0 1 0 0 0
0 0 0 0 0 0 1 0 0 0 0
0 0 0 1 0 0 0 0 0 0 0
0 0 0 1 0 0 0 0 0 0 0
0 0 0 0 0 1 0 0 0 0 0"""

    val fileName = "testGraph.txt"

    new PrintWriter(fileName) {
      write(fileContent)
      close
    }

    Graph.fromFile(fileName) shouldBe testGraph

    new File(fileName).delete
  }
}
