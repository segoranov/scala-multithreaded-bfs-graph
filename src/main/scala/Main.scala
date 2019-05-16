package graph

object GraphApp {

  val usage = """
    Usage: java -jar <file.jar> -i <graph-file.in> -t <number of tasks> -o <graph-data.out> -q
           java -jar <file.jar> -n <number_of_vertices> -t <number of tasks> -o <graph-data.out> -q

    The parameters -i and -n are mutually exclusive. Only one of the two should be present.
    The parameter -o is optional - if not present, no results will be written to a file.
    The parameter -q (quiet) is optional - if present, only the total time for the BFS will be written to the output file.
  """

  def main(args: Array[String]): Unit = {

    if (args.isEmpty) {
      println(usage)
    }

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
