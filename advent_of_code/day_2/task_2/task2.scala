/*
 * Based on your calculations, the planned course doesn't seem to make any
 * sense. You find the submarine manual and discover that the process is
 * actually slightly more complicated.
 *
 * In addition to horizontal position and depth, you'll also need to track a
 * third value, aim, which also starts at 0. The commands also mean something
 * entirely different than you first thought:
 *
 *  down X increases your aim by X units.
 *   up X decreases your aim by X units.
 *   forward X does two things:
 *       It increases your horizontal position by X units.
 *       It increases your depth by your aim multiplied by X.
 *
 * Again note that since you're on a submarine, down and up do the opposite of
 * what you might expect: "down" means aiming in the positive direction.
 *
 * Now, the above example does something different:
 *
 *   forward 5 adds 5 to your horizontal position, a total of 5.
 *   Because your aim is 0, your depth does not change.
 *
 *   down 5 adds 5 to your aim, resulting in a value of 5.
 *
 *   forward 8 adds 8 to your horizontal position, a total of 13.
 *   Because your aim is 5, your depth increases by 8*5=40.
 *
 *   up 3 decreases your aim by 3, resulting in a value of 2.
 *
 *   down 8 adds 8 to your aim, resulting in a value of 10.
 *
 *   forward 2 adds 2 to your horizontal position, a total of 15.
 *   Because your aim is 10, your depth increases by 2*10=20 to a total of 60.
 *
 * After following these new instructions, you would have a horizontal position
 * of 15 and a depth of 60. (Multiplying these produces 900.)
 *
 * Using this new interpretation of the commands, calculate the horizontal
 * position and depth you would have after following the planned course. What do
 * you get if you multiply your final horizontal position by your final depth?
 *
 */
import scala.io.Source
import scala.annotation.tailrec

type Aim = Int
type Movement = (Dir, Int)
type Movements = List[Movement]

enum Dir:
  case Forward, Up, Down

def parseDirection(line: String): Movement =
  val List(dirStr, valueStr) = (line split "\\s+").toList
  val value = Integer parseInt valueStr
  dirStr.trim match
    case "forward" => (Dir.Forward, value)
    case "down"    => (Dir.Down, value)
    case "up"      => (Dir.Up, value)

def inputToList(filename: String): Movements =
  Source.fromFile(filename)
    .getLines.toList
    .filter(_.nonEmpty)
    .map(parseDirection)

def calcDirsProduct(movements: Movements): Int =
  @tailrec
  def iter(aim: Int, hpos: Int, depth: Int, mvmntsLeft: Movements): Int =
    mvmntsLeft match
      case Nil                    => hpos * depth
      case (Dir.Down, x) :: xs    => iter(aim + x, hpos, depth, xs)
      case (Dir.Up, x) :: xs      => iter(aim - x, hpos, depth, xs)
      case (Dir.Forward, x) :: xs => iter(aim, hpos + x, depth + (x * aim), xs)

  iter(0, 0, 0, movements)

@main def app(filename: String): Unit = println {
  calcDirsProduct compose inputToList apply filename
}

