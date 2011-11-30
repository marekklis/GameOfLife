package gameoflife

/**
 *
 * User: Marek Klis
 * Date: 30.11.11 21:16
 */

import scala.collection._

trait Status

case object ALIVE extends Status {
  override def toString = "#"
}

case object DEAD extends Status {
  override def toString = "-"
}

case class Point(x: Int, y: Int)

class Board(activeCells: Set[Point], size: Int) {

  var board = Array.fill[Status](size, size)(DEAD)

  def printBoard = {
    board foreach (i => {
      i foreach (j => print(j))
      println("")
    }
      )
  }

  def turn = {
    val newBoard = Array.fill[Status](size, size)(DEAD)
    for (i <- 0 until board.size; j <- 0 until board(i).size) {
      val numberOfNeighbors = neighbors(Point(i, j))
      if ((numberOfNeighbors == 3) ||
        ((numberOfNeighbors == 2) && (board(i)(j) == ALIVE))) newBoard(i)(j) = ALIVE
    }
    board = newBoard
  }

  def neighbors(point: Point) = {
    def possibleNeighbors() = {
      Set[Point](
        Point(point.x - 1, point.y + 1), Point(point.x, point.y + 1), Point(point.x + 1, point.y + 1),
        Point(point.x - 1, point.y), Point(point.x + 1, point.y),
        Point(point.x - 1, point.y - 1), Point(point.x, point.y - 1), Point(point.x + 1, point.y - 1))
    }
    possibleNeighbors().filter {
      p => ((0 until size) contains p.x) &&
        ((0 until size) contains p.y) &&
        (board(p.x)(p.y) == ALIVE)
    }.size
  }

  activeCells filter (p => ((0 until size) contains p.x) &&
    ((0 until size) contains p.y)) foreach (p => board(p.x)(p.y) = ALIVE)
}

object GameOfLife extends App {

  val aliveCells = Set(
    Point(0, 0),
    Point(0, 1),
    Point(0, 2),
    Point(4, 4),
    Point(4, 5),
    Point(4, 6)
  )

  val generations = 5
  val boardSize = 10

  var board = new Board(aliveCells, boardSize)

  for (i <- 1 to generations) {
    println("generation: " + i)
    board.printBoard
    board turn
  }

}