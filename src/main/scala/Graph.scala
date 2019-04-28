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
    // TODO: see if below checks are ok. Handle exceptions functionally instead of throwing.
    if (numberOfThreads <= 0) {
      throw new IllegalArgumentException("Number of threads in bfs traversal must be at least 1!")
    }

    if (numberOfThreads > getNumVertices) {
      throw new IllegalArgumentException("Number of threads cannot be higher than the number of vertices!")
    }

    var allTasks: Set[Future[(Path, ElapsedMilliSeconds)]] = Set.empty

    List.range(0, numberOfThreads - 1).foreach(threadNumber => {
      // start new asynchronous task
      val f: Future[(Path, ElapsedMilliSeconds)] = Future {
        println("Thread " + threadNumber + " starts BFS from vertex " + threadNumber)

        // use threadNumber as number of vertex
        time {
          bfsTraversalFrom(threadNumber)
        }
      }

      // callback - when the task is completed
      f onComplete {
        case Success(result) => println("Thread " + threadNumber
          + " finished. Time elapsed milliseconds: " + result._2
          + "; bfs result: " + result._1)
        case Failure(t) => println("An error has occurred in thread " + threadNumber + ": " + t.getMessage)
      }

      allTasks += f
    })

    // continue synchronous work on main thread
    List.range(numberOfThreads - 1, getNumVertices).foreach(vertex => {
      println("Main thread starts BFS from vertex " + vertex)
      val result = time {
        bfsTraversalFrom(vertex)
      }

      println("Main thread finished. Time elapsed milliseconds: " + result._2
        + "; bfs result: " + result._1)
    })

    // wait for all futures to complete before exiting
    println("Waiting for all futures to complete")
    allTasks.foreach(future => Await.ready(future, Duration.Inf))
  }

  def bfsTraversalFrom(start: Vertex): Path = {
//    Thread.sleep(300)

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
}
