package graph

import graph.Graph.{AdjMatrix, Row}
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

    val testGraphManyVertices = Graph.withRandomEdges(numberOfVertices = 200)

    val result1 = time {
      testGraphManyVertices.bfsTraversalStartingFromAllVertices(numberOfTasks = 1)
    }

    val result2 = time {
      testGraphManyVertices.bfsTraversalStartingFromAllVertices(numberOfTasks = 10)
    }

    val result3 = time {
      testGraphManyVertices.bfsTraversalStartingFromAllVertices(numberOfTasks = 20)
    }

    println("Time taken for all the tasks to finish in milliseconds: 1 thread -> " + result1._2
      + "; 10 threads -> " + result2._2 + "; 20 threads -> " + result3._2)
  }
}
