
import scala.io.Source
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import Integer.parseInt
import javax.swing.plaf.basic.BasicBorders.MarginBorder

val boardSize = 5

type Board = List[List[Cell]]
type Scores = List[Int]

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

  val boards = tables.sliding(boardSize, boardSize)
    .map { fiveRows =>
      fiveRows. map { row =>
        val splits = row split "\\s+"
        splits.toList
          .filter(_.nonEmpty)
          .map(Integer.parseInt)
          .map(Cell(_, false))
      }.toList
    }.toList

  (scores, boards)



case class Cell(var number: Int, var marked: Boolean):
  def mark = marked = true
  def unmark = marked = false
  def isMarked = marked
  override def toString: String =
    val value = if !marked then number.toString else "x"
    value.padTo(2, ' ')


// supply number to board
extension (board: Board)

  /*
   * Return the transponed version of matrix
   */
  private def transponed(): Board =
    val transponed = for (col <- 0 until boardSize)
      yield board.map(row => row.apply(col))
    transponed.toList


  /**
    * Supplies number to the matrix so it could be
    * Marked if exist
    */
  def supply(number: Int): Unit =
    for row <- board
        cell <- row do
      if cell.number == number then
        cell.mark


  def wins: Boolean =
    def hasHorScore(b: Board)  = b.exists(_.forall(_.marked))
    def hasVertScore           = hasHorScore(transponed())
    hasHorScore(board) || hasVertScore


  def calculateWinScore(winningNumber: Int): Int =
    val nonWinningCells = board.map { row =>
      row.foldLeft (0) { case (acc, next) =>
        if !next.marked
          then acc + next.number
          else acc + 0
      }
    }
    nonWinningCells.sum * winningNumber


def calcWinScore(scores: List[Int], boards: List[Board]): Option[Int] =
  for number <- scores do
    for board <- boards do
      board.supply(number)
      if board.wins then
        return Some(board.calculateWinScore(number))
  None


def main(args: Array[String]): Unit = println {
  calcWinScore.tupled(readInput(taskInput))
}

