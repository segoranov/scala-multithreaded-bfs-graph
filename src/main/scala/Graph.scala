package graph

import graph.Graph._

import scala.annotation.tailrec
import scala.collection.immutable.Queue

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

  def bfsTraversalFrom(start: Vertex): List[Vertex] = {

    @tailrec
    def bfs(toVisit: Queue[Vertex], reached: Set[Vertex], path: List[Vertex]): List[Vertex] = {
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

  def apply(adjMatrix: AdjMatrix): Graph = {
    def checkMatrixValidity = {
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

    checkMatrixValidity
    new Graph(adjMatrix)
  }
}

object MyTest {
  def main(args: Array[String]): Unit = {
    val testGraph = Graph(AdjMatrix(
      Row(0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0),
      Row(1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0),
      Row(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Row(1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0),
      Row(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
      Row(1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1),
      Row(0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0),
      Row(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0),
      Row(0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0),
      Row(0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0),
      Row(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0)
    ))

    println(testGraph.bfsTraversalFrom(0))

  }
}


