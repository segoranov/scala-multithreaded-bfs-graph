package graph

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

object CommandLineArgumentsParser extends StrictLogging {

  def parse(args: List[String]): Option[CommandLineArgumentsData] =
    if (!areMandatoryParametersPresent(args)) {
      logger.error("Not all mandatory parameters are present!")
      None
    }
    else if (!areMutuallyExclusiveParametersCorrect(args)) {
      logger.error("The parameters -i and -n are mutually exclusive. Only one of the two should be present.")
      None
    }
    else createMapFromCommandLineArguments(Map.empty, args) match {
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

  type ArgumentsToValuesMap = Map[String, String]

  import NumberOfTasksFormats._

  @tailrec
  private def createMapFromCommandLineArguments(map: ArgumentsToValuesMap, list: List[String]): Option[ArgumentsToValuesMap] = list match {
    case Nil => Some(map)
    case "-i" :: graphInputFile :: tail => createMapFromCommandLineArguments(map ++ Map("-i" -> graphInputFile), tail)
    case "-t" :: numberOfTasks :: tail => createMapFromCommandLineArguments(map ++ Map("-t" -> numberOfTasks), tail)
    case "-o" :: graphOutputFile :: tail => createMapFromCommandLineArguments(map ++ Map("-o" -> graphOutputFile), tail)
    case "-n" :: numberOfVertices :: tail => createMapFromCommandLineArguments(map ++ Map("-n" -> numberOfVertices), tail)
    case "-q" :: tail => createMapFromCommandLineArguments(map ++ Map("-q" -> ""), tail)
    case "--help" :: _ | "-h" :: _ => None
    case option :: _ => {
      logger.error("Unknown option " + option)
      None
    }
  }

  private def areValuesInMapValid(argumentsToValuesMap: ArgumentsToValuesMap): Boolean = {
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

  private def parseNumberOfTasks(numberOfTasks: String): Option[List[Int]] = {
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
  private def areNumberOfTasksInValidFormat(numberOfTasks: String) = getFormatOfNumberOfTasks(numberOfTasks) match {
    case JustNumber => true
    case CommaSeparated => areCommaFormattedTasksValid(numberOfTasks)
    case DashSeparated => areDashFormattedTasksValid(numberOfTasks)
    case Invalid => false
  }

  private def areDashFormattedTasksValid(numberOfTasks: String) = {
    val rangeOfTasks = numberOfTasks.split("-")

    rangeOfTasks.size == 2 &&
      isValidInteger(rangeOfTasks(0)) &&
      isValidInteger(rangeOfTasks(1)) &&
      rangeOfTasks(0).toInt < rangeOfTasks(1).toInt
  }

  private def areCommaFormattedTasksValid(numberOfTasks: String) =
    numberOfTasks.split(",").forall(isValidInteger)

  private def getFormatOfNumberOfTasks(numberOfTasks: String) = {
    if (isValidInteger(numberOfTasks)) JustNumber
    else if (numberOfTasks.contains("-")) DashSeparated
    else if (numberOfTasks.contains(",")) CommaSeparated
    else Invalid
  }

  private def areMutuallyExclusiveParametersCorrect(args: List[String]) = !(args.contains("-n") && args.contains("-i"))

  private def areMandatoryParametersPresent(args: List[String]) = (args.contains("-i") || args.contains("-n")) && args.contains("-t")

  private def isValidInteger(str: String): Boolean =
    try {
      str.toInt
      true
    }
    catch {
      case _: Throwable =>
        false
    }
}