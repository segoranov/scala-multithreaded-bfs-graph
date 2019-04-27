package graph

import graph.Graph._

case class Graph(adjMatrix: AdjMatrix) {

  def getVertices = List.range(0, adjMatrix.size)

  def hasVertex(v: Vertex) = 0 <= v && v <= adjMatrix.size - 1
  def isEdge(v1: Vertex, v2: Vertex) = hasVertex(v1) && hasVertex(v2) && adjMatrix(v1)(v2) == 1


  // def bfsTraversal(start: Int, end: Int, neighbours: Int => List[Int]): Queue = ???
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
    val testGraph = Graph(AdjMatrix(Row(0,0,1), Row(0,0,0), Row(0,0,0)))


  }
}


