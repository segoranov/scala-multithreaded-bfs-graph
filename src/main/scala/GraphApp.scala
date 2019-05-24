package graph

import java.io.PrintWriter
import java.nio.file.{Files, Paths}

import com.typesafe.scalalogging.LazyLogging
import graph.Graph.BFSTraversalFromAllVerticesResult

import scala.annotation.tailrec

case class CommandLineArgumentsData(numberOfTasks: Int,
                                    outputFile: Option[String],
                                    inputFile: Option[String],
                                    numberOfVertices: Option[Int],
                                    runQuietly: Boolean)

object GraphApp extends LazyLogging {
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
              numberOfTasks = argumentsToValuesMap("-t").toInt,
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

  def areValuesInMapValid(map: ArgumentsToValuesMap) = {

    def isValidInteger(str: String): Boolean = {
      try {
        str.toInt
        true
      }
      catch {
        case _: Throwable => {
          logger.error(str + " is not a valid integer!")
          false
        }
      }
    }

    val numberOfTasksIsValid = isValidInteger(map("-t"))

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

    numberOfTasksIsValid && numberOfVerticesIsValid && inputFileIsValid
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
                                 (implicit graph: Graph, results: BFSTraversalFromAllVerticesResult) = {
    val pw = new PrintWriter(outputFile)

    pw.write("Graph adjacency matrix:\n")
    pw.write(graph.toString)

    pw.write("\n\nTotal time taken to generate below BFS traversals: " + results._2 + " milliseconds\n")
    pw.write("All generated BFS traversals:\n")
    results._1.foreach(resultFromTask => pw.write(resultFromTask.generatedBFSPath.mkString("Traversal:\n", " ", "\n\n")))
    pw.close
  }

  def writeQuietlyAlgorithmResultsToFile(outputFile: String = "graph-bfs-quiet-result.txt")
                                        (implicit results: BFSTraversalFromAllVerticesResult) = {
    val pw = new PrintWriter(outputFile)
    pw.write("Total time elapsed in milliseconds: ")
    pw.write(results._2.toString)
    pw.close
  }

  def main(args: Array[String]): Unit = {
    processCommandLineArguments(args.toList) match {
      case None => println(usage)
      case Some(commandLineArgumentsData) => {
        implicit val graph = createGraphFromCommandLineArguments(commandLineArgumentsData)

        implicit val resultFromAlgorithm = graph.bfsTraversalStartingFromAllVertices(commandLineArgumentsData.numberOfTasks)

        if (commandLineArgumentsData.outputFile.isDefined) {
          writeAlgorithmResultsToFile(commandLineArgumentsData.outputFile.get)
        }

        if (commandLineArgumentsData.runQuietly) {
          writeQuietlyAlgorithmResultsToFile()
        }
      }
    }

    // TODO:
    //  Finish documentation - detailed and descriptive!
  }
}
