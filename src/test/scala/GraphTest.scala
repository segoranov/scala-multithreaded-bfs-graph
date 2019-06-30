package graph

import java.io.{File, PrintWriter}

import graph.Graph.{AdjMatrix, Row}
import org.scalatest.{FlatSpec, Matchers}

class GraphTest extends FlatSpec with Matchers {

  def isMatrixSymmetrical(adjMatrix: AdjMatrix): Boolean = {
    val s = adjMatrix.size

    for (i <- 0 until s; j <- 0 until s) {
      if (adjMatrix(i)(j) != adjMatrix(j)(i)) {
        return false
      }
    }

    true
  }

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

  val testGraphManyVertices = Graph.withRandomEdges(numberOfVertices = 400)

  "testGraph adjacency matrix" should "be symmetrical" in {
    isMatrixSymmetrical(testGraph.adjMatrix) shouldBe true
  }

  "withRandomEdges" should "always create symmetrical matrix" in {
    for (i <- 1 to 100) {
      isMatrixSymmetrical(Graph.withRandomEdges(i + 50).adjMatrix) shouldBe true
    }
  }

  "all vertices" should "be from 0 to 10" in {
    testGraph.getVertices shouldBe (0 to 10).toSet
    testGraph.getNumVertices shouldBe 11
  }

  "vertices" should "be in graph" in {
    (0 to 10).foreach(vertex => testGraph.hasVertex(vertex) shouldBe true)
  }

  "vertices" should "not be in graph" in {
    (-10 to -1).foreach(vertex => testGraph.hasVertex(vertex) shouldBe false)
    (11 to 30).foreach(vertex => testGraph.hasVertex(vertex) shouldBe false)
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
    type TimeElapsedInMilliseconds = Long

    // test only if we have 3 CPUs or more, otherwise threads will interfere with one another
    if (Runtime.getRuntime.availableProcessors >= 3) {
      val timeTaken = (1 to Runtime.getRuntime.availableProcessors - 2)
        .map(numberOfThreads =>
          testGraphManyVertices.bfsTraversalStartingFromAllVertices(numberOfThreads).timeForCompletionInMilliseconds)

      // should not contain dupliactes,
      // because this means that different number of threads perform equally
      val containsDuplicates = timeTaken.size != timeTaken.distinct.size
      containsDuplicates shouldBe false

      // more threads should perform faster than less threads,
      // therefore the sequence must be sorted in descending order
      timeTaken shouldBe timeTaken.sorted(Ordering.Long.reverse)
    }
  }

  "reading graph from file" should "be correct" in {
    new PrintWriter(testGraphFileName) {
      write(testGraphFileContent)
      close
    }

    Graph.fromFile(testGraphFileName).adjMatrix shouldBe testGraph.adjMatrix

    new File(testGraphFileName).delete
  }

  "writing graph to file" should "have correct format" in {
    testGraph.writeToFile(testGraphFileName)

    Graph.fromFile(testGraphFileName).adjMatrix shouldBe testGraph.adjMatrix

    new File(testGraphFileName).delete
  }

  "graphs" should "not be equal" in {
    testGraph.writeToFile(testGraphFileName)

    val anotherGraphFileContent =
      """11
0 1 1 1 1 1 0 0 0 0 0
1 0 0 0 0 0 1 0 1 0 0
1 0 0 0 0 0 0 0 0 0 0
1 0 0 0 0 0 0 0 1 1 0
1 0 0 0 0 1 0 0 0 0 0
1 0 0 0 1 0 0 0 0 0 1
0 1 0 0 0 0 0 1 0 0 0
0 0 0 0 0 0 1 0 0 0 0
0 0 0 1 0 0 0 0 0 0 0
0 0 0 1 0 0 0 0 0 0 0
0 0 0 0 0 1 0 0 0 0 0"""

    new PrintWriter("another") {
      write(anotherGraphFileContent)
      close
    }

    Graph.fromFile(testGraphFileName).adjMatrix should not be Graph.fromFile("another").adjMatrix

    new File(testGraphFileName).delete
    new File("another").delete
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
      Graph.fromFile(testGraphFileName).adjMatrix shouldBe testGraph.adjMatrix
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
    CommandLineArgumentsParser.parse(List("-i", "graph-in.txt", "-n", "1024", "-t", "25")) shouldBe None
    CommandLineArgumentsParser.parse(List("-i", "graph-in.txt", "-n", "1024", "-t", "")) shouldBe None
    CommandLineArgumentsParser.parse(List("-i", "graph-in.txt", "-n", "1024")) shouldBe None
    CommandLineArgumentsParser.parse(List("-i", "__NON_FUCKING_EXISTENT_FILE___", "-t", "25")) shouldBe None
    CommandLineArgumentsParser.parse(List("-n", "125", "-q", "-t", "25asd")) shouldBe None
    CommandLineArgumentsParser.parse(List("-n", "125", "-q", "-t", "1- 20")) shouldBe None
    CommandLineArgumentsParser.parse(List("-n", "125", "-q", "-t", "1,2, 3,4,5")) shouldBe None
  }

  it should "be valid" in {
    CommandLineArgumentsParser.parse(List("-n", "125", "-o", "output.txt", "-t", "25")) should not be None
    CommandLineArgumentsParser.parse(List("-n", "125", "-t", "25")) should not be None
    CommandLineArgumentsParser.parse(List("-n", "125", "-q", "-t", "25")) should not be None
    CommandLineArgumentsParser.parse(List("-n", "125", "-q", "-t", "1-20")) should not be None
    CommandLineArgumentsParser.parse(List("-n", "125", "-q", "-t", "1,2,3,4,5")) should not be None
  }

  "BFS traversal results" should "be the same regardless of the number of threads" in {
    testGraph.bfsTraversalStartingFromAllVertices(7).allResults.map(_.generatedBFSTraversal) shouldBe
      testGraph.bfsTraversalStartingFromAllVertices(1).allResults.map(_.generatedBFSTraversal)
  }
}
