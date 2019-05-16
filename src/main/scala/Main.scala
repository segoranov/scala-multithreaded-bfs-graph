package graph

object Test {

  def main(args: Array[String]): Unit = {

    val testGraphManyVertices = Graph.withRandomEdges(numberOfVertices = 200)

//    testGraphManyVertices.bfsTraversalStartingFromAllVertices(numberOfTasks = 1)
//    testGraphManyVertices.bfsTraversalStartingFromAllVertices(numberOfTasks = 2)
//    testGraphManyVertices.bfsTraversalStartingFromAllVertices(numberOfTasks = 3)
//    testGraphManyVertices.bfsTraversalStartingFromAllVertices(numberOfTasks = 4)
    testGraphManyVertices.bfsTraversalStartingFromAllVertices(numberOfTasks = 10)
//    testGraphManyVertices.bfsTraversalStartingFromAllVertices(numberOfTasks = 15)
//    testGraphManyVertices.bfsTraversalStartingFromAllVertices(numberOfTasks = 20)
//    testGraphManyVertices.bfsTraversalStartingFromAllVertices(numberOfTasks = 25)
//    testGraphManyVertices.bfsTraversalStartingFromAllVertices(numberOfTasks = 32)
//    testGraphManyVertices.bfsTraversalStartingFromAllVertices(numberOfTasks = 40)
//    testGraphManyVertices.bfsTraversalStartingFromAllVertices(numberOfTasks = 80)

    // TODO:
    //  Command line parameters - see requirements
    //  Documentation - detailed and desriptive!
  }
}
