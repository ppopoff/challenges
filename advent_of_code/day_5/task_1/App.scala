/*
 * You come across a field of hydrothermal vents on the ocean floor!
 * These vents constantly produce large, opaque clouds, so it would be best to
 * avoid them if possible.
 *
 * They tend to form in lines; the submarine helpfully produces a list of
 * nearby lines of vents (your puzzle input) for you to review. For example:
 *
 * 0,9 -> 5,9
 * 8,0 -> 0,8
 * 9,4 -> 3,4
 * 2,2 -> 2,1
 * 7,0 -> 7,4
 * 6,4 -> 2,0
 * 0,9 -> 2,9
 * 3,4 -> 1,4
 * 0,0 -> 8,8
 * 5,5 -> 8,2
 *
 * Each line of vents is given as a line segment in the format
 * x1,y1 -> x2,y2 where x1,y1 are the coordinates of one end the line segment
 * and x2,y2 are the coordinates of the other end. These line segments include
 * the points at both ends. In other words:
 *
 * An entry like 1,1 -> 1,3 covers points 1,1, 1,2, and 1,3.
 * An entry like 9,7 -> 7,7 covers points 9,7, 8,7, and 7,7.
 *
 * For now, only consider horizontal and vertical lines: lines where
 * either x1 = x2 or y1 = y2.
 *
 * So, the horizontal and vertical lines from the above list would produce the
 * following diagram:
 *
 * .......1..
 * ..1....1..
 * ..1....1..
 * .......1..
 * .112111211
 * ..........
 * ..........
 * ..........
 * ..........
 * 222111....
 *
 * In this diagram, the top left corner is 0,0 and the bottom right corner
 * is 9,9. Each position is shown as the number of lines which cover that
 * point or . if no line covers that point. The top-left pair of 1s, for
 * example, comes from 2,2 -> 2,1; the very bottom row is formed by the
 * overlapping lines 0,9 -> 5,9 and 0,9 -> 2,9.
 *
 * To avoid the most dangerous areas, you need to determine the number of
 * points where at least two lines overlap. In the above example, this is
 * anywhere in the diagram with a 2 or larger - a total of 5 points.
 *
 * Consider only horizontal and vertical lines. At how many points do at
 * least two lines overlap?
 *
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
    case (LineDirection.Vertical, baseX, (y1, y2))   =>
      (y1 to y2).toList.map(y => (baseX, y))
    case (LineDirection.Horizontal, baseY, (x1, x2)) =>
      (x1 to x2).toList.map(x => (x, baseY))


def countIntersections(coordsByFrequencies: Map[Coord, Int]): Int =
  coordsByFrequencies.count { case (coord, freq) => freq > 1 }


def main(args: Array[String]): Unit = println {
  val lines = readInput(taskFile)
  val (straight, diagonal) = separateInput(lines)

  // get map of (dot -> occurances)
  val straightCoordsMap =
    straight.flatMap(straightLineToDots).howFrequent

  countIntersections(straightCoordsMap)
}

