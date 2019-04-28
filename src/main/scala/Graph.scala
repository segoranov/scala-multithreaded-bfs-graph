package graph

import graph.Graph._

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global
import Timer._

import scala.concurrent.duration.Duration

case class Graph(adjMatrix: AdjMatrix) {

  def getVertices: Set[Vertex] = if (adjMatrix.isEmpty) Set.empty else List.range(0, adjMatrix.size).toSet

  def getNumVertices = adjMatrix.size

  def hasVertex(v: Vertex) = 0 <= v && v <= adjMatrix.size - 1

  def isEdge(v1: Vertex, v2: Vertex): Either[String, Boolean] = {
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

  def bfsTraversalStartingFromAllVertices(numberOfThreads: Int) = {
    if (numberOfThreads <= 0) {
      throw new IllegalArgumentException("Number of threads in bfs traversal cannot be less than 1!")
    }

    val tasksForEachThread = getVerticesForEachThreadToWorkOn(numberOfThreads)

    println("Lists of vertices for each thread to work on: " + tasksForEachThread)

    import scala.collection.mutable.Set

    var actualThreadsUsed: Set[String] = Set.empty
    var allFutures: Set[Future[(ResultFromTask, TimeElapsedInMilliseconds, ThreadID)]] = Set.empty

    tasksForEachThread.foreach(listOfVertices => {
      val future = startNewTask(listOfVertices)
      allFutures += future

      future onComplete {
        case Success(result) => {
          actualThreadsUsed += result._3
          println("Thread " + result._3
            + " finished. Time elapsed milliseconds: " + result._2
            + "; bfs result: " + result._1)
        }
        case Failure(t) => println("An error has occurred: " + t.getMessage)
      }
    })

    allFutures.foreach(future => Await.ready(future, Duration.Inf))

    // give some time for the callbacks to print the results; probably not the best way but for now it works
    Thread.sleep(1500)

    println("Number of actual threads used in the computation: " + actualThreadsUsed.size)
  }

  def bfsTraversalFrom(start: Vertex): Path = {
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

  private def startNewTask(listOfVertices: List[Vertex]): Future[(ResultFromTask, TimeElapsedInMilliseconds, ThreadID)] = {
    Future {
      val result = time {
        listOfVertices.foldLeft[List[Path]](List.empty)((acc, curVertex) => {
          println("Thread " + Thread.currentThread.getName.split("-").last + " starts BFS from vertex " + curVertex)
          bfsTraversalFrom(curVertex) :: acc
        })
      }

      (result._1.reverse, result._2, Thread.currentThread.getName.split("-").last)
    }
  }

  private def getVerticesForEachThreadToWorkOn(numberOfThreads: Int): List[List[Vertex]] = {
    var tasksForEachThread = List.range(0, getNumVertices).grouped(getNumVertices / numberOfThreads).toList

    if (tasksForEachThread.size > numberOfThreads) {
      tasksForEachThread = tasksForEachThread.dropRight(2) :+ (tasksForEachThread.dropRight(1).last ++ tasksForEachThread.last)
    }

    tasksForEachThread
  }

}

case object Graph {
  type Row = List[Int]

  def Row(xs: Int*) = List(xs: _*)

  type AdjMatrix = List[Row]

  def AdjMatrix(xs: Row*) = List(xs: _*)

  type Vertex = Int
  type Path = List[Vertex]

  private type ThreadID = String
  private type ResultFromTask = List[Path]

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
}

