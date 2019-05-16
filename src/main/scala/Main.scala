package graph

object GraphApp {

  val usage =
    """
    Usage: java -jar <file.jar> -i <graph-file.in> -t <number of tasks> [-o <graph-data.out>] [-q]
           java -jar <file.jar> -n <number_of_vertices> -t <number of tasks> [-o <graph-data.out>] [-q]

    Mandatory parameters:
    -i -> input file with adjacency matrix to read graph from
    -n -> number of vertices in order to create graph with random generated edges
    -t -> number of tasks for the BFS algorithm

    The parameters -i and -n are mutually exclusive. Only one of the two should be present.

    Optional parameters:
      -o - output file for results. If not present, no results will be written to an output file.
      -q (quiet) - If present, only the total time for the BFS will be written to the output file. Discarded if -o is not present.
  """

  def checkCommandLineArgumentsAreValid(args: List[String]) = {
    if (args.contains("-n") && args.contains("-i")) {
      println("The parameters -i and -n are mutually exclusive. Only one of the two should be present.")
      System.exit(1)
    }

    if ((!args.contains("-i") && !args.contains("-n")) || !args.contains("-t")) {
      println("Not all mandatory parameters are present!")
      System.exit(1)
    }
  }

  def main(args: Array[String]): Unit = {

    if (args.isEmpty) {
      println(usage)
      System.exit(0)
    }

    val arglist = args.toList
    checkCommandLineArgumentsAreValid(arglist)

    type OptionMap = Map[Symbol, Any]

    def nextOption(map: OptionMap, list: List[String]): OptionMap = {
      list match {
        case Nil => map
        case "-i" :: graphInputFile :: tail => nextOption(map ++ Map('i -> graphInputFile), tail)
        case "-t" :: numberOfTasks :: tail => nextOption(map ++ Map('t -> numberOfTasks.toInt), tail)
        case "-o" :: graphOutputFile :: tail => nextOption(map ++ Map('o -> graphOutputFile), tail)
        case "-n" :: numberOfVertices :: tail => nextOption(map ++ Map('n -> numberOfVertices.toInt), tail)
        case "-q" :: tail => nextOption(map ++ Map('q -> true), tail)
        case "--help" :: _ | "-h" :: _ => {
          println(usage)
          System.exit(0)
          Map.empty
        }
        case option :: _ => {
          println("Unknown option " + option)
          System.exit(1)
          Map.empty
        }
      }
    }

    val options = nextOption(Map.empty, arglist)

    println(options)

    val runQuietly = options.contains('q)
    val readGraphFromFile = options.contains('i)

    val numberOfTasks = options('t).asInstanceOf[Int]

    if (readGraphFromFile) {
      val file = options('i).asInstanceOf[String]
      val graph = Graph.fromFile(file)

      graph.bfsTraversalStartingFromAllVertices(numberOfTasks)
    }
    else {
      val numberOfVertices = options('n).asInstanceOf[Int]
      val graph = Graph.withRandomEdges(numberOfVertices)

      graph.bfsTraversalStartingFromAllVertices(numberOfTasks)
    }

    // TODO:
    //  Implement -q and -o, currently they are just ignored.
    //  Documentation - detailed and desriptive!
  }
}
