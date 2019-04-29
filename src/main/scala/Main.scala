package graph

import graph.Graph.{AdjMatrix, Row}
import scala.util.Random
import Timer.time

object Test {

  def main(args: Array[String]): Unit = {
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

    val testGraphManyVertices = Graph(List.fill(2000)(List.fill(2000)(Random.nextInt(2))))

    val result = time {
      testGraph.bfsTraversalStartingFromAllVertices(numberOfThreads = 4)
    }

    println("Time taken for all the tasks to finish in milliseconds: " + result._2)
  }
}
