/*
 * You're already almost 1.5km (almost a mile) below the surface of the ocean,
 * already so deep that you can't see any sunlight. What you can see, however,
 * is a giant squid that has attached itself to the outside of your submarine.
 *
 * Maybe it wants to play bingo?
 * Bingo is played on a set of boards each consisting of a 5x5 grid of numbers.
 * Numbers are chosen at random, and the chosen number is marked on all boards
 * on which it appears. (Numbers may not appear on all boards.) If all numbers
 * in any row or any column of a board are marked, that board wins.
 * (Diagonals don't count.)
 *
 * The submarine has a bingo subsystem to help passengers (currently, you and
 * the giant squid) pass the time. It automatically generates a random order in
 * which to draw numbers and a random set of boards (your puzzle input).
 *
 * For example:
 * 7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
 *
 * 22 13 17 11  0
 * 8  2 23  4 24
 * 21  9 14 16  7
 *  6 10  3 18  5
 *  1 12 20 15 19
 *
 *  3 15  0  2 22
 *  9 18 13 17  5
 * 19  8  7 25 23
 * 20 11 10 24  4
 * 14 21 16 12  6
 *
 * 14 21 17 24  4
 * 10 16 15  9 19
 * 18  8 23 26 20
 * 22 11 13  6  5
 *  2  0 12  3  7
 *
 * After the first five numbers are drawn (7, 4, 9, 5, and 11), there are no
 * winners, but the boards are marked as follows (shown here adjacent to each
 * other to save space):
 *
 * 22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
 *  8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
 * 21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
 *  6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
 *  1 12 20 15 19        14 21 16 12  6         2  0 12  3  7
 *
 * After the next six numbers are drawn (17, 23, 2, 0, 14, and 21),
 * there are still no winners:
 *
 * 22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
 *  8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
 * 21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
 *  6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
 *  1 12 20 15 19        14 21 16 12  6         2  0 12  3  7
 *
 * Finally, 24 is drawn:
 *
 * 22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
 *  8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
 * 21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
 *  6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
 *  1 12 20 15 19        14 21 16 12  6         2  0 12  3  7
 *
 * At this point, the third board wins because it has at least one complete row
 * or column of marked numbers (in this case, the entire top row is
 * marked: 14 21 17 24 4).
 *
 * The score of the winning board can now be calculated. Start by finding the
 * sum of all unmarked numbers on that board; in this case, the sum is 188.
 * Then, multiply that sum by the number that was just called when the board
 * won, 24, to get the final score, 188 * 24 = 4512.
 *
 * To guarantee victory against the giant squid, figure out which board will
 * win first. What will your final score be if you choose that board?
 */
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

