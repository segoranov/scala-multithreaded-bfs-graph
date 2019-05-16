package graph

object Test {

  def main(args: Array[String]): Unit = {

    val testGraphManyVertices = Graph.withRandomEdges(numberOfVertices = 300)

    for(numberOfTasks <- (1 to Runtime.getRuntime.availableProcessors)) {
      testGraphManyVertices.bfsTraversalStartingFromAllVertices(numberOfTasks)
    }

    // testGraphManyVertices.bfsTraversalStartingFromAllVertices(numberOfTasks = 10)

    // TODO:
    //  Command line parameters - see requirements
    //  Documentation - detailed and desriptive!
  }
}
