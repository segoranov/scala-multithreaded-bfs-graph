package graph

import java.io.PrintWriter
import java.nio.file.{Files, Paths}

import com.typesafe.scalalogging.StrictLogging

import scala.annotation.tailrec

case class CommandLineArgumentsData(numberOfTasks: List[Int],
                                    outputFile: Option[String],
                                    inputFile: Option[String],
                                    numberOfVertices: Option[Int],
                                    runQuietly: Boolean)

object NumberOfTasksFormats extends Enumeration {
  type Format = Value
  val DashSeparated, CommaSeparated, JustNumber, Invalid = Value
}

object GraphApp extends StrictLogging {

  import NumberOfTasksFormats._

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

  def processCommandLineArguments(args: List[String]): Option[CommandLineArgumentsData] =
    if (args.isEmpty) None
    else if (!areMandatoryParametersPresent(args)) {
      logger.error("Not all mandatory parameters are present!")
      None
    }
    else if (!areMutuallyExclusiveParametersCorrect(args)) {
      logger.error("The parameters -i and -n are mutually exclusive. Only one of the two should be present.")
      None
    }
    else parseCommandLineArguments(Map.empty, args) match {
      case Some(argumentsToValuesMap) => if (areValuesInMapValid(argumentsToValuesMap)) {
        Some(CommandLineArgumentsData(
          numberOfTasks = parseNumberOfTasks(argumentsToValuesMap("-t")).get,
          numberOfVertices = argumentsToValuesMap.get("-n").map(_.toInt),
          inputFile = argumentsToValuesMap.get("-i"),
          outputFile = argumentsToValuesMap.get("-o"),
          runQuietly = argumentsToValuesMap.contains("-q")))
      }
      else None
      case _ => None
    }

  @tailrec
  def parseCommandLineArguments(map: ArgumentsToValuesMap, list: List[String]): Option[ArgumentsToValuesMap] = list match {
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

  def areValuesInMapValid(argumentsToValuesMap: ArgumentsToValuesMap): Boolean = {
    val numberOfVerticesIsValid = argumentsToValuesMap.get("-n").map(isValidInteger) match {
      case Some(false) => {
        logger.error("Invalid number of vertices!")
        false
      }
      case _ => true
    }

    val inputFileIsValid = argumentsToValuesMap.get("-i").map(file => Files.exists(Paths.get(file))) match {
      case Some(false) => {
        logger.error("Graph input file '" + argumentsToValuesMap("-i") + "' does not exist!")
        false
      }
      case _ => true
    }

    if (!areNumberOfTasksInValidFormat(argumentsToValuesMap("-t"))) {
      logger.error("Invalid format for number of tasks is used!")
      false
    } else {
      numberOfVerticesIsValid && inputFileIsValid
    }
  }

  def parseNumberOfTasks(numberOfTasks: String): Option[List[Int]] = {
    if (!areNumberOfTasksInValidFormat(numberOfTasks)) None
    else getFormatOfNumberOfTasks(numberOfTasks) match {
      case JustNumber => Some(List(numberOfTasks.toInt))
      case DashSeparated => {
        val rangeOfTasks = numberOfTasks.split("-")
        Some((rangeOfTasks(0).toInt to rangeOfTasks(1).toInt).toList)
      }
      case CommaSeparated => Some(numberOfTasks.split(",").map(_.toInt).toList)
      case Invalid => None
    }
  }

  /* Valid formats about number of tasks:
          1st format (just a number): 15 -> run BFS with 15 threads
          2nd format (dash separated): 1-20 -> run BFS with number of threads 1, 2, 3, 4, ... , 20
          3rd format (comma separated): 1,5,20,14 -> run BFS with 1, 5, 20, 14 number of threads
   */
  def areNumberOfTasksInValidFormat(numberOfTasks: String) = getFormatOfNumberOfTasks(numberOfTasks) match {
    case JustNumber => true
    case CommaSeparated => areCommaFormattedTasksValid(numberOfTasks)
    case DashSeparated => areDashFormattedTasksValid(numberOfTasks)
    case Invalid => false
  }

  def areDashFormattedTasksValid(numberOfTasks: String) = {
    val rangeOfTasks = numberOfTasks.split("-")

    rangeOfTasks.size == 2 &&
      isValidInteger(rangeOfTasks(0)) &&
      isValidInteger(rangeOfTasks(1)) &&
      rangeOfTasks(0).toInt < rangeOfTasks(1).toInt
  }

  def areCommaFormattedTasksValid(numberOfTasks: String) =
    numberOfTasks.split(",").forall(isValidInteger)

  def getFormatOfNumberOfTasks(numberOfTasks: String) = {
    if (isValidInteger(numberOfTasks)) JustNumber
    else if (numberOfTasks.contains("-")) DashSeparated
    else if (numberOfTasks.contains(",")) CommaSeparated
    else Invalid
  }

  def areMutuallyExclusiveParametersCorrect(args: List[String]) = !(args.contains("-n") && args.contains("-i"))

  def areMandatoryParametersPresent(args: List[String]) = (args.contains("-i") || args.contains("-n")) && args.contains("-t")

  type ArgumentsToValuesMap = Map[String, String]

  def isValidInteger(str: String): Boolean =
    try {
      str.toInt
      true
    }
    catch {
      case _: Throwable =>
        false
    }

  def createGraphFromCommandLineArguments(commandLineArgumentsData: CommandLineArgumentsData) =
    if (commandLineArgumentsData.inputFile.isDefined) {
      Graph.fromFile(commandLineArgumentsData.inputFile.get)
    }
    else {
      Graph.withRandomEdges(commandLineArgumentsData.numberOfVertices.get)
    }

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

  def main(args: Array[String]): Unit = processCommandLineArguments(args.toList) match {
    case None => println(usage)
    case Some(commandLineArgumentsData) => {
      implicit val graph = createGraphFromCommandLineArguments(commandLineArgumentsData)

      implicit val resultsFromAlgorithm = commandLineArgumentsData.numberOfTasks.map(graph.bfsTraversalStartingFromAllVertices)

      if (commandLineArgumentsData.outputFile.isDefined) {
        writeAlgorithmResultsToFile(commandLineArgumentsData.outputFile.get)
      }

      if (commandLineArgumentsData.runQuietly) {
        writeQuietlyAlgorithmResultsToFile()
      }

      // print quiet results to screen for convenience
      println(quietResultToString)
    }
  }
}
