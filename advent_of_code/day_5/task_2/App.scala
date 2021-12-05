/*
 * Unfortunately, considering only horizontal and vertical lines doesn't give you the full picture; you need to
 * also consider diagonal lines.
 *
 * Because of the limits of the hydrothermal vent mapping system, the lines in your list will only ever be horizontal,
 * vertical, or a diagonal line at exactly 45 degrees. In other words:
 *
 * An entry like 1,1 -> 3,3 covers points 1,1, 2,2, and 3,3.
 * An entry like 9,7 -> 7,9 covers points 9,7, 8,8, and 7,9.
 * Considering all lines from the above example would now produce the following diagram:
 *
 * 1.1....11.
 * .111...2..
 * ..2.1.111.
 * ...1.2.2..
 * .112313211
 * ...1.2....
 * ..1...1...
 * .1.....1..
 * 1.......1.
 * 222111....
 * You still need to determine the number of points where at least two lines overlap. In the above example,
 * this is still anywhere in the diagram with a 2 or larger - now a total of 12 points.
 *
 * Consider all of the lines. At how many points do at least two lines overlap?
 */
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import Tools._

type Pos = Int
type XPos = Int
type YPos = Int
type Coord = (XPos, YPos)
type Line = (Coord, Coord)
type Lines = List[Line]
type Vector = (Int, Int)

enum LineDirection:
  case Horizontal, Vertical

def readInput(fname: FileName): Lines =
  val linesStr: List[String] = flines(fname)
  linesStr.map { line =>
    val List(srcCoord, dstCoord) =
      line.split("\\s+->\\s+").toList.map { coord =>
        val Array(x, y) = coord.split(',')
        (x.toInt, y.toInt)
      }
    (srcCoord, dstCoord)
  }


def isStaightLine(line: Line): Boolean =
  val ((x1, y1), (x2, y2)) = line
  x1 == x2 || y1 == y2


def separateInput(lines: Lines): (Lines, Lines) =
  lines partition isStaightLine


// Lines may go in directions that can be considered
// Negative. That's not good for plotting
def lineToDirection(line: Line): (LineDirection, Pos, Coord) =
  val ((x1, y1), (x2, y2)) = line
  if x1 == x2 then
    val from = math.min(y1, y2)
    val to   = math.max(y1, y2)
    (LineDirection.Vertical, x1, (from , to))
  else
    val from = math.min(x1, x2)
    val to   = math.max(x1, x2)
    (LineDirection.Horizontal, y1, (from, to))


// Maps horizontal lines to a list of dots
def straightLineToDots(line: Line): List[Coord] =
  lineToDirection(line) match
    case (LineDirection.Vertical, baseX, (y1, y2)) =>
      (y1 to y2).toList.map(y => (baseX, y))
    case (LineDirection.Horizontal, baseY, (x1, x2)) =>
      (x1 to x2).toList.map(x => (x, baseY))


def diagonalLinesToDots(line: Line): List[Coord] =
  val ((x1, y1), (x2, y2)) = line
  def mul(n0: Int, n1: Int): Int =
    if n0 > n1 then -1 else 1

  val xs = x1 to x2 by mul.tupled(x1, x2)
  val ys = y1 to y2 by mul.tupled(y1, y2)
  (xs zip ys).toList


def countIntersections(coordsByFrequencies: Map[Coord, Int]): Int =
  coordsByFrequencies.count { case (coord, freq) => freq > 1 }


def main(args: Array[String]): Unit = println {
  val lines = readInput(taskFile)
  val (straight, diagonal) = separateInput(lines)
  val straightCoords = straight.flatMap(straightLineToDots)
  val diagonalCoords = diagonal.flatMap(diagonalLinesToDots)
  val allCoords = straightCoords ++ diagonalCoords
  countIntersections(allCoords.howFrequent)
}
