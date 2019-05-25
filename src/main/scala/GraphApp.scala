package graph

import java.io.PrintWriter
import java.nio.file.{Files, Paths}

import com.typesafe.scalalogging.LazyLogging

import scala.annotation.tailrec

case class CommandLineArgumentsData(numberOfTasks: List[Int],
                                    outputFile: Option[String],
                                    inputFile: Option[String],
                                    numberOfVertices: Option[Int],
                                    runQuietly: Boolean)

object GraphApp extends LazyLogging {
  val usage =
    """
    Usage: java -jar <file.jar> -i <graph-file.in> -t <number of tasks> [-o <output_file_name>] [-q]
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

  def processCommandLineArguments(args: List[String]): Option[CommandLineArgumentsData] = {
    if (args.isEmpty) {
      None
    }
    else if (!areMandatoryParametersPresent(args)) {
      logger.error("Not all mandatory parameters are present!")
      None
    }
    else if (!areMutuallyExclusiveParametersCorrect(args)) {
      logger.error("The parameters -i and -n are mutually exclusive. Only one of the two should be present.")
      None
    }
    else {
      parseCommandLineArguments(Map.empty, args) match {
        case Some(argumentsToValuesMap) => {
          if (areValuesInMapValid(argumentsToValuesMap)) {
            Some(CommandLineArgumentsData(
              numberOfTasks = parseNumberOfTasks(argumentsToValuesMap("-t")),
              numberOfVertices = if (argumentsToValuesMap.contains("-n")) Some(argumentsToValuesMap("-n").toInt) else None,
              inputFile = argumentsToValuesMap.get("-i"),
              outputFile = argumentsToValuesMap.get("-o"),
              runQuietly = argumentsToValuesMap.contains("-q")))
          }
          else {
            None
          }
        }
        case _ => None
      }
    }
  }

  def areMutuallyExclusiveParametersCorrect(args: List[String]) = !(args.contains("-n") && args.contains("-i"))

  def areMandatoryParametersPresent(args: List[String]) = (args.contains("-i") || args.contains("-n")) && args.contains("-t")

  type ArgumentsToValuesMap = Map[String, String]

  @tailrec
  def parseCommandLineArguments(map: ArgumentsToValuesMap, list: List[String]): Option[ArgumentsToValuesMap] = {
    list match {
      case Nil => Some(map)
      case "-i" :: graphInputFile :: tail => parseCommandLineArguments(map ++ Map("-i" -> graphInputFile), tail)
      case "-t" :: numberOfTasks :: tail => parseCommandLineArguments(map ++ Map("-t" -> numberOfTasks), tail)
      case "-o" :: graphOutputFile :: tail => parseCommandLineArguments(map ++ Map("-o" -> graphOutputFile), tail)
      case "-n" :: numberOfVertices :: tail => parseCommandLineArguments(map ++ Map("-n" -> numberOfVertices), tail)
      case "-q" :: tail => parseCommandLineArguments(map ++ Map("-q" -> ""), tail)
      case "--help" :: _ | "-h" :: _ => None
      case option :: _ => {
        logger.error("Unknown option " + option)
        None
      }
    }
  }

  def parseNumberOfTasks(numberOfTasks: String): List[Int] = {
    /* Valid formats about number of tasks:
            15 -> run BFS with 15 threads
            1-20 -> run BFS with number of threads 1, 2, 3, 4, ... , 20
            1,5,20,14 -> run BFS with 1, 5, 20, 14 number of threads
    */

    if (isValidInteger(numberOfTasks)) {
      List(numberOfTasks.toInt)
    }
    else if (numberOfTasks.contains("-")) {
      // format: x-y
      val rangeOfTasks = numberOfTasks.split("-")

      val rangeStart = rangeOfTasks(0).toInt
      val rangeEnd = rangeOfTasks(1).toInt

      (rangeStart to rangeEnd).toList
    } else if (numberOfTasks.contains(",")) {
      // format: x,y,z,...
      numberOfTasks.split(",").map(_.toInt).toList
    } else {
      List.empty
    }
  }

  def isValidInteger(str: String): Boolean = {
    try {
      str.toInt
      true
    }
    catch {
      case _: Throwable => {
        false
      }
    }
  }

  def areValuesInMapValid(map: ArgumentsToValuesMap): Boolean = {
    val numberOfVerticesIsValid = {
      if (map.contains("-n")) {
        isValidInteger(map("-n"))
      }
      else {
        true
      }
    }

    val inputFileIsValid = {
      if (map.contains("-i")) {
        val inputFile = map("-i")

        val fileExists = Files.exists(Paths.get(inputFile))

        if (!fileExists) {
          logger.error("Graph input file '" + inputFile + "' does not exist!")
        }

        fileExists
      }
      else {
        true
      }
    }

    areNumberOfTasksInValidFormat(map("-t")) && numberOfVerticesIsValid && inputFileIsValid
  }

  def areNumberOfTasksInValidFormat(numberOfTasks: String) = {

    /* Valid formats about number of tasks:
            15 -> run BFS with 15 threads
            1-20 -> run BFS with number of threads 1, 2, 3, 4, ... , 20
            1,5,20,14 -> run BFS with 1, 5, 20, 14 number of threads
    */

    if (isValidInteger(numberOfTasks)) {
      true
    }
    else if (numberOfTasks.contains("-")) {
      // format: x-y
      val rangeOfTasks = numberOfTasks.split("-")

      if (rangeOfTasks.size != 2) {
        // must be 2 if format is x-y
        logger.error("Invalid range in number of tasks specified! Format must be 'x-y' - one dash and two numbers around it, specifying a valid range.")
        false
      } else {
        if (!isValidInteger(rangeOfTasks(0)) || !isValidInteger(rangeOfTasks(1))) {
          logger.error("Invalid range in number of tasks specified!")
          false
        } else {
          val rangeStart = rangeOfTasks(0).toInt
          val rangeEnd = rangeOfTasks(1).toInt

          if (rangeStart > rangeEnd) {
            logger.error("Invalid range in number of tasks specified! " + rangeStart + " is greater than " + rangeEnd + "!")
            false
          } else {
            true
          }
        }
      }
    } else if (numberOfTasks.contains(",")) {
      // format: x,y,z,...
      val arrNumberOfTasks = numberOfTasks.split(",")

      if (arrNumberOfTasks.exists(!isValidInteger(_))) {
        logger.error("Invalid number of tasks specified!")
        false
      } else {
        true
      }
    } else {
      logger.error("Invalid range in number of tasks specified!")
      false
    }
  }

  def createGraphFromCommandLineArguments(commandLineArgumentsData: CommandLineArgumentsData) = {
    if (commandLineArgumentsData.inputFile.isDefined) {
      Graph.fromFile(commandLineArgumentsData.inputFile.get)
    }
    else {
      Graph.withRandomEdges(commandLineArgumentsData.numberOfVertices.get)
    }
  }

  def writeAlgorithmResultsToFile(outputFile: String)
                                 (implicit graph: Graph,
                                  results: List[BFSTraversalFromAllVerticesResult]) = {
    val pw = new PrintWriter(outputFile)

    results.foreach(r => pw.write(
      "Time for completion in milliseconds with " + r.numberOfThreads + " threads: "
        + r.timeForCompletionInMilliseconds.toString + "\n"))

    pw.write("\n--------------------- DETAILS ---------------------\n")

    pw.write("Graph adjacency matrix:\n")
    pw.write(graph.toString + "\n\n")


    pw.write("All generated BFS traversals:\n")
    results.head.allResults.foreach(r => pw.write(r.generatedBFSTraversal.mkString("Traversal:\n", " ", "\n\n")))

    pw.close
  }

  def writeQuietlyAlgorithmResultsToFile(outputFile: String = "graph-bfs-quiet-result.txt")
                                        (implicit results: List[BFSTraversalFromAllVerticesResult]) = {
    val pw = new PrintWriter(outputFile)

    results.foreach(r => pw.write(
      "Time for completion in milliseconds with " + r.numberOfThreads + " threads: "
        + r.timeForCompletionInMilliseconds.toString + "\n"))

    pw.close
  }

  def main(args: Array[String]): Unit = {
    processCommandLineArguments(args.toList) match {
      case None => println(usage)
      case Some(commandLineArgumentsData) => {
        implicit val graph = createGraphFromCommandLineArguments(commandLineArgumentsData)

        //implicit val resultsFromAlgorithm = graph.bfsTraversalStartingFromAllVertices(commandLineArgumentsData.numberOfTasks)
        implicit val resultsFromAlgorithm = commandLineArgumentsData.numberOfTasks.map(graph.bfsTraversalStartingFromAllVertices)

        if (commandLineArgumentsData.outputFile.isDefined) {
          writeAlgorithmResultsToFile(commandLineArgumentsData.outputFile.get)
        }

        if (commandLineArgumentsData.runQuietly) {
          writeQuietlyAlgorithmResultsToFile()
        }

        graph.writeToFile("test-graph-adjacency-matrix.txt")
      }
    }

    // TODO:
    //  Finish documentation - detailed and descriptive!
  }
}
