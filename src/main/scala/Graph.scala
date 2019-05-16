package graph

import java.util.concurrent.Executors

import graph.Graph._
import graph.Timer._

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Random
import com.typesafe.scalalogging._

case class ResultFromTask(generatedBFSPath: Path, timeForCompletionInMilliseconds: TimeElapsedInMilliseconds, threadID: Long)

case class Graph(adjMatrix: AdjMatrix) extends LazyLogging {

  def getVertices: Set[Vertex] = if (adjMatrix.isEmpty) Set.empty else List.range(0, adjMatrix.size).toSet

  def getNumVertices = adjMatrix.size

  def hasVertex(v: Vertex) = 0 <= v && v <= adjMatrix.size - 1

  def hasEdge(v1: Vertex, v2: Vertex): Either[String, Boolean] = {
    if (!hasVertex(v1))
      Left("Vertex " + v1 + " is not in the graph.")
    else if (!hasVertex(v2))
      Left("Vertex " + v2 + " is not in the graph.")
    else
      Right(adjMatrix(v1)(v2) == 1)
  }

  def getNeighbours(v: Vertex): Either[String, Set[Vertex]] = {
    if (hasVertex(v)) {
      Right(List.range(0, getNumVertices).filter(v1 => adjMatrix(v)(v1) == 1).toSet)
    }
    else {
      Left("No such vertex in the graph!")
    }
  }

  def printAdjMatrix = adjMatrix.foreach(println)

  def bfsTraversalStartingFromAllVertices(numberOfTasks: Int): (Set[ResultFromTask], TimeElapsedInMilliseconds) = {
    logger.debug("Starting BFS traversal from all vertices (" + getNumVertices + ") with number of tasks: " + numberOfTasks)

    val threadPool = Executors.newFixedThreadPool(numberOfTasks)
    implicit val ec = ExecutionContext.fromExecutor(threadPool)

    if (numberOfTasks < 1) {
      throw new IllegalArgumentException("Number of tasks for bfs traversal cannot be less than 1!")
    }

    val result = time {
      getVertices.map(start_BFS_task_from_vertex(_)).map(Await.result(_, Duration.Inf))
    }

    logger.debug("Total number of threads used in current run: " + result._1.map(_.threadID).size)
    logger.debug("Total time elapsed (milliseconds) in current run: " + result._2)

    threadPool.shutdown

    result
  }

  private[graph] def bfsTraversalFrom(start: Vertex): Path = {
    @tailrec
    def bfs(toVisit: Queue[Vertex], reached: Set[Vertex], path: Path): Path = {
      if (toVisit.isEmpty) path
      else {
        val current = toVisit.head
        val newNeighbours = getNeighbours(current).right.get.filter(!reached(_))

        bfs(
          toVisit.dequeue._2.enqueue(newNeighbours),
          reached ++ newNeighbours,
          current :: path
        )
      }
    }

    bfs(Queue(start), Set(start), List.empty).reverse
  }

  private def start_BFS_task_from_vertex(startingVertex: Vertex)(implicit ec: ExecutionContext): Future[ResultFromTask] = Future {
    logger.debug("Start BFS from vertex " + startingVertex)
    val result = time {
      bfsTraversalFrom(startingVertex)
    }

    logger.debug("Finish BFS started from vertex " + startingVertex
      + ". Time elapsed in milliseconds: " + result._2)

    ResultFromTask(result._1, result._2, Thread.currentThread.getName.split("-").last.toLong)
  }
}

case object Graph {
  type Row = List[Int]

  def Row(xs: Int*) = List(xs: _*)

  type AdjMatrix = List[Row]

  def AdjMatrix(xs: Row*) = List(xs: _*)

  type Vertex = Int
  type Path = List[Vertex]

  def apply(adjMatrix: AdjMatrix): Graph = {
    def checkAdjMatrixValidity = {
      adjMatrix.foreach(row => {
        if (row.size != adjMatrix.size)
          throw new IllegalArgumentException("Adjacency matrix has incorrect dimensions!")
        else row.foreach(vertex => {
          if (vertex != 0 && vertex != 1)
            throw new IllegalArgumentException(
              "Incorrect value in the matrix. Each value in the matrix should be either 1 or 0!")
        })
      })
    }

    checkAdjMatrixValidity
    new Graph(adjMatrix)
  }

  // sample file format for graph with 3 vertices:
  // 3
  // 0 1 0
  // 1 0 1
  // 0 0 1
  def fromFile(file: String) = {
    val fileSource = scala.io.Source.fromFile(file)
    val fileContent = fileSource.getLines.toList

    fileSource.close

    val adjMatrix =
      fileContent.tail // size of the matrix is not needed, ignore it
        .map(_.split(" "))
        .map(_.toList)
        .map(row => row.map(_.toInt))

    new Graph(adjMatrix)
  }

  def withRandomEdges(numberOfVertices: Int) = {
    if (numberOfVertices < 0) {
      throw new IllegalArgumentException("Graph cannot have negative number of vertices!")
    }

    new Graph(List.fill(numberOfVertices)(List.fill(numberOfVertices)(Random.nextInt(2))))
  }
}

