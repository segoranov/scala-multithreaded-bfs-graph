package graph

import java.io.{File, PrintWriter}

import graph.Graph.{AdjMatrix, Row}
import graph.Timer.TimeElapsedInMilliseconds
import org.scalatest.{FlatSpec, Matchers}

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

  val testGraphFileContent =
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

  val testGraphFileName = "testGraph.txt"

  val testGraphManyVertices = Graph.withRandomEdges(numberOfVertices = 300)

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
    testGraph.hasEdge(0, 1) shouldBe Right(true)
    testGraph.hasEdge(0, 0) shouldBe Right(false)

    testGraph.getVertices.foreach(vertex => {
      if (vertex == 5)
        testGraph.hasEdge(10, vertex) shouldBe Right(true)
      else
        testGraph.hasEdge(10, vertex) shouldBe Right(false)
    })

    testGraph.hasEdge(-5, 205) shouldBe Left("Vertex " + -5 + " is not in the graph.")
  }

  "testGraph" should "have 11 vertices" in {
    testGraph.getNumVertices shouldBe 11
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

    // create no more tasks than the number of available processors - 1, it is of no use
    val mapNumberOfThreadsToTimeElapsed =
      List.range(1, Runtime.getRuntime.availableProcessors)
        .foldLeft[Map[NumberOfTasks, TimeElapsedInMilliseconds]](Map.empty)((acc, numberOfTasks) => {

        val millisecondsElapsed = testGraphManyVertices.bfsTraversalStartingFromAllVertices(numberOfTasks)._2
        acc + (numberOfTasks -> millisecondsElapsed)
      })

    // less threads should spent more time on the task
    mapNumberOfThreadsToTimeElapsed.foreach(pair => {
      mapNumberOfThreadsToTimeElapsed
        .filter(other => other._1 < pair._1)
        .foreach(other => other._2 should be > pair._2)
    })
  }

  "reading graph from file" should "be correct" in {
    new PrintWriter(testGraphFileName) {
      write(testGraphFileContent)
      close
    }

    Graph.fromFile(testGraphFileName) shouldBe testGraph

    new File(testGraphFileName).delete
  }

  "writing graph to file" should "have correct format" in {
    testGraph.writeToFile(testGraphFileName)

    Graph.fromFile(testGraphFileName) shouldBe testGraph

    new File(testGraphFileName).delete
  }

  "reading graph from incorrectly formatted file" should "throw IllegalArgumentException exception" in {
    val invalidGraphFileContent =
      """14
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

    new PrintWriter(testGraphFileName) {
      write(invalidGraphFileContent)
      close
    }

    assertThrows[IllegalArgumentException] {
      Graph.fromFile(testGraphFileName) shouldBe testGraph
    }
  }

  "creating graph with negative number of vertices" should "throw IllegalArgumentException exception" in {
    assertThrows[IllegalArgumentException] {
      Graph.withRandomEdges(-5)
    }
  }

  "empty graph" should "have 0 vertices" in {
    Graph.empty.getNumVertices shouldBe 0
  }

  "command line arguments" should "be invalid in" in {
    GraphApp.processCommandLineArguments(List("-i", "graph-in.txt", "-n", "1024", "-t", "25")) shouldBe None
    GraphApp.processCommandLineArguments(List("-i", "graph-in.txt", "-n", "1024", "-t", "")) shouldBe None
    GraphApp.processCommandLineArguments(List("-i", "graph-in.txt", "-n", "1024")) shouldBe None
    GraphApp.processCommandLineArguments(List("-i", "__NON_FUCKING_EXISTENT_FILE___", "-t", "25")) shouldBe None
    GraphApp.processCommandLineArguments(List("-n", "125", "-q", "-t", "25asd")) shouldBe None
  }

  it should "be valid" in {
    GraphApp.processCommandLineArguments(List("-n", "125", "-o", "output.txt", "-t", "25")) should not be None
    GraphApp.processCommandLineArguments(List("-n", "125", "-t", "25")) should not be None
    GraphApp.processCommandLineArguments(List("-n", "125", "-q", "-t", "25")) should not be None
  }
}
