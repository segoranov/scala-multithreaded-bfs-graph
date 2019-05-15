package graph

import graph.Graph.{AdjMatrix, Row}

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

    testGraphManyVertices.bfsTraversalStartingFromAllVertices(numberOfTasks = 1)
    testGraphManyVertices.bfsTraversalStartingFromAllVertices(numberOfTasks = 2)
    testGraphManyVertices.bfsTraversalStartingFromAllVertices(numberOfTasks = 3)
    testGraphManyVertices.bfsTraversalStartingFromAllVertices(numberOfTasks = 4)
    testGraphManyVertices.bfsTraversalStartingFromAllVertices(numberOfTasks = 10)
    testGraphManyVertices.bfsTraversalStartingFromAllVertices(numberOfTasks = 15)
    testGraphManyVertices.bfsTraversalStartingFromAllVertices(numberOfTasks = 20)
    testGraphManyVertices.bfsTraversalStartingFromAllVertices(numberOfTasks = 25)
    testGraphManyVertices.bfsTraversalStartingFromAllVertices(numberOfTasks = 32)
    testGraphManyVertices.bfsTraversalStartingFromAllVertices(numberOfTasks = 40)
    testGraphManyVertices.bfsTraversalStartingFromAllVertices(numberOfTasks = 80)


    // QUESTIONS:
    // 0. Is my logic correct: If all threads in the pool are busy,
    // Futures (tasks) will wait in a queue for the thread pool to free some thread.

    // 1. Why the fuck with 100 tasks is faster than with 4 tasks???

    // 2. How come when I start 100 tasks, the program uses 100 threads, when I have 4 cores?

    // 3. Why is the program not terminating - probably some thread is still running, but which one???

    // TODO:
    //  Logging to a file - thread stars, thread finishes, etc...
    //  Command line parameters
  }
}
