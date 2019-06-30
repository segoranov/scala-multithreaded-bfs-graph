package graph

import java.io.PrintWriter
import com.typesafe.scalalogging.StrictLogging

object GraphApp extends StrictLogging {

  val usage =
    """
    Usage: java -jar <file.jar> -i <graph-file.in> -t <number of tasks> [-o <output_file_name>] [-q]

              OR

           java -jar <file.jar> -n <number_of_vertices> -t <number of tasks> [-o <output_file_name>] [-q]

    Mandatory parameters:
      -i -> input file with adjacency matrix to read graph from
      -n -> number of vertices in order to create graph with random generated edges
      -t -> number of tasks for the BFS algorithm

    There are 3 formats for specifying number of tasks:
      1) Just a number. Example:
        -t 15
       This will run the algorithm with 15 threads.

      2) Range format. Specify a valid range with a dash. Example:
        -t 1-20
       This will run the algorithm with 1 thread, then with 2 threads, and so on and so on till 20.

      3) Number of threads separated by comma. Example:
        -t 1,3,5,7,9
       This will run the algorithm with 1 thread, then with 3 threads, and so on...


    Sample input file format for graph with 3 vertices:

      graph-input.txt:
      3
      0 1 0
      1 0 1
      0 0 1

    The parameters -i and -n are mutually exclusive. Only one of the two should be present.

    Optional parameters:
      -o - output file for results. If not present, no results will be written to an output file.
      -q (quiet) - if present, only the total time taken for the BFS will be written to an output file with name 'graph-bfs-quiet-result.txt'.

    The program will always log information in file named 'graph-bfs.log' such as:

    10:25:37.124 [pool-1-thread-5] DEBUG graph.Graph - Start BFS from vertex 10
    10:25:37.339 [pool-1-thread-5] DEBUG graph.Graph - Finish BFS started from vertex 10. Time elapsed in milliseconds: 210

    and also log errors such as:

    10:40:17.914 [main] ERROR graph.GraphApp$ - Not all mandatory parameters are present!
  """

  def createGraph(commandLineArgumentsData: CommandLineArgumentsData) =
    if (commandLineArgumentsData.inputFile.isDefined) Graph.fromFile(commandLineArgumentsData.inputFile.get)
    else Graph.withRandomEdges(commandLineArgumentsData.numberOfVertices.get)

  def writeAlgorithmResultsToFile(outputFile: String)
                                 (implicit graph: Graph,
                                  results: List[BFSTraversalFromAllVerticesResult]) = {
    val pw = new PrintWriter(outputFile)

    pw.write(quietResultToString)

    pw.write("\n\n--------------------- DETAILS ---------------------\n")

    pw.write("Graph adjacency matrix:\n")
    pw.write(graph.toString + "\n\n")


    pw.write("All generated BFS traversals:\n")
    results.head.allResults.foreach(r => pw.write(r.generatedBFSTraversal.mkString("Traversal:\n", " ", "\n\n")))

    pw.close
  }

  def writeQuietlyAlgorithmResultsToFile(outputFile: String = "graph-bfs-quiet-result.txt")
                                        (implicit results: List[BFSTraversalFromAllVerticesResult]) =
    new PrintWriter(outputFile) {
      write(quietResultToString)
      close
    }

  def quietResultToString(implicit results: List[BFSTraversalFromAllVerticesResult]) =
    results.map(r => r.numberOfThreads + " threads -> " + r.timeForCompletionInMilliseconds + " ms").reduceLeft(_ + "\n" + _)

  def main(args: Array[String]): Unit = CommandLineArgumentsParser.parse(args.toList) match {
    case None => println(usage)
    case Some(commandLineArgumentsData) => {
      implicit val graph = createGraph(commandLineArgumentsData)

      implicit val resultsFromAlgorithm = commandLineArgumentsData.numberOfTasks.map(graph.bfsTraversalStartingFromAllVertices)

      if (commandLineArgumentsData.outputFile.isDefined)
        writeAlgorithmResultsToFile(commandLineArgumentsData.outputFile.get)

      if (commandLineArgumentsData.runQuietly) writeQuietlyAlgorithmResultsToFile()

      // print quiet results to screen for convenience
      println(quietResultToString)
    }
  }
}
