/**
 * Suppose the lanternfish live forever and have unlimited food and space. Would they take over the entire ocean?
 * After 256 days in the example above, there would be a total of 26984457539 lanternfish!
 * How many lanternfish would there be after 256 days?
 * Your puzzle answer was 1675781200288.
 */
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import Input._
import Tools._

def trace(day: Int, fishes: Map[Long, Long]): Long = {
  day match
    case 0 => fishes.values.sum
    case _ =>
      val shifted = shiftFishes(fishes)
      // after the shift we lost some fishes with the countdown of 6
      // that should retured back to the pride
      val lostGeneration = fishes.getOrElse(0L, 0L)
      val currentFertiles = shifted.getOrElse(6L, 0L) + lostGeneration
      val allfishes = shifted ++ Map(6L -> currentFertiles)
      trace(day - 1, allfishes)
}


/**
 *  Shifts goddamn fishes:
 *  (5 <-> 999)  to (4 <-> 999)
 *  (6 <-> 888)  to (5 <-> 888)
 *  (3 <-> 777)  to (2 <-> 777)
 *  (0 <-> 123)  to (8 <-> 123)
 */
def shiftFishes(oldFishes: Map[Long, Long]): Map[Long, Long] =
  @tailrec
  def rec (newFishes: List[(Long, Long)], oldFishes: List[(Long, Long)]): Map[Long, Long] =
    oldFishes.toList match
      case Nil                        => newFishes.toMap
      case (0L, count) :: rest        => rec((8L, count) :: newFishes, rest)
      case (countDown, count) :: rest => rec((countDown - 1, count) :: newFishes, rest)

  rec(List.empty, oldFishes.toList)


def main(args: Array[String]): Unit = println {
  val fishes: Map[Long, Long] = Input.taskInput
    .map(_.toLong)
    .groupMapReduce(identity)(_ => 1L)(_ + _)

  trace(256, fishes)
}

