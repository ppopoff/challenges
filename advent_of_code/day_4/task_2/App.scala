
import scala.io.Source
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import Integer.parseInt
import javax.swing.plaf.basic.BasicBorders.MarginBorder


val boardSize = 5

type Scores = List[Int]
type BoardState = List[List[Cell]]

case class Board(state: BoardState, var isWon: Boolean = false):
  def markAsWon = isWon = true
  def hasNotYetWon = !isWon

def testInput = "./test.input"
def taskInput = "./task.input"


def readInput(filename: String): (Scores, List[Board]) =
  val lines = Source.fromFile(filename)
    .getLines.toList
    .filter(_.nonEmpty)

  val scoresLine :: tables = lines
  val scores =
    scoresLine.split(",").toList
      .filter(_.nonEmpty)
      .map(Integer.parseInt)

  val rawBoards =
    tables.sliding(boardSize, boardSize).map { fiveRows =>
      fiveRows. map { row =>
        (row split "\\s+").toList.filter(_.nonEmpty)
          .map(Integer.parseInt)
          .map(num => Cell(num, false))
      }.toList
    }.toList

  (scores, rawBoards.map(rb => Board(state = rb, isWon = false)))


case class Cell(var number: Int, var marked: Boolean):
  def mark = marked = true
  def unmark = marked = false
  def isMarked = marked


extension (board: Board)
  private def transponed(): List[List[Cell]] =
    val transponed = for (col <- 0 until boardSize)
      yield board.state.map(row => row.apply(col))
    transponed.toList

  def supply(number: Int): Unit =
    for row <- board.state
        cell <- row do
      if cell.number == number then
        cell.mark

  def wins: Boolean =
    def hasHorScore(b: BoardState): Boolean  = b.exists(_.forall(_.marked))
    def hasVertScore: Boolean = hasHorScore(transponed())
    hasHorScore(board.state) || hasVertScore

  def calculateWinScore(winningNumber: Int): Int =
    val nonWinningCells = board.state.map { row =>
      row.foldLeft(0) { case (acc, next) =>
        if !next.marked
          then acc + next.number
          else acc + 0
      }
    }

    nonWinningCells.sum * winningNumber


def calcWinScore(scores: List[Int], boards: List[Board]): Option[Int] = {
  for number <- scores do
    for board <- boards if board.hasNotYetWon do
      board supply number
      if board.wins then
        val wonBoards = boards.count(_.isWon)
        val allBoards = boards.length

        if (allBoards - wonBoards) == 1 then return Some {
          board calculateWinScore number
        }
        else
          board.markAsWon
  None
}


def main(args: Array[String]): Unit = println {
  calcWinScore.tupled(readInput(taskInput))
}
