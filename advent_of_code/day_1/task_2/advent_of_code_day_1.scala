/*
 * Considering every single measurement isn't as useful as you expected: there's just too much noise in the data.
 *
 * Instead, consider sums of a three-measurement sliding window. Again considering the above example:
 *
 * 199  A
 * 200  A B
 * 208  A B C
 * 210    B C D
 * 200  E   C D
 * 207  E F   D
 * 240  E F G
 * 269    F G H
 * 260      G H
 * 263        H
 *
 * Start by comparing the first and second three-measurement windows. The measurements in the first window are marked A (199, 200, 208); their sum is 199 + 200 + 208 = 607. The second window is marked B (200, 208, 210); its sum is 618. The sum of measurements in the second window is larger than the sum of the first, so this first comparison increased.
 *
 * Your goal now is to count the number of times the sum of measurements in this sliding window increases from the previous sum. So, compare A with B, then compare B with C, then C with D, and so on. Stop when there aren't enough measurements left to create a new three-measurement sum.
 *
 * In the above example, the sum of each three-measurement window is as follows:
 *
 * A: 607 (N/A - no previous sum)
 * B: 618 (increased)
 * C: 618 (no change)
 * D: 617 (decreased)
 * E: 647 (increased)
 * F: 716 (increased)
 * G: 769 (increased)
 * H: 792 (increased)
 *
 * In this example, there are 5 sums that are larger than the previous sum.
 *
 * Consider sums of a three-measurement sliding window. How many sums are larger than the previous sum?
 */

import scala.io.Source
import scala.annotation.tailrec

enum State:
  case Increased, Decreased, NA, NoChange

def depthsToState(measurements: List[Int]): List[State] =
  @tailrec
  def rec(prev: Int, converted: List[State], mms: List[Int]): List[State] =
    if converted.isEmpty && mms.nonEmpty && prev == 0 then                                      // The first run: setting the N/A measurement
      rec(mms.head, List(State.NA), mms.tail)
    else if mms.isEmpty then
      converted
    else mms match
      case head :: tail if head == prev =>
        rec(head, State.NoChange :: converted, tail)
      case head :: tail if head >= prev =>
        rec(head, State.Increased :: converted, tail)
      case head :: tail if head < prev  =>
        rec(head, State.Decreased :: converted, tail)
      case _ =>
        rec(prev, converted, mms)

  val firstMeasurment :: restMeasurements = measurements
  rec(0, Nil, measurements)


def linesToInput(filename: String): List[Int] =
  Source.fromFile(filename)
    .getLines.toList
    .filter(_.nonEmpty)
    .map(Integer.parseInt)



@main def app(filename: String = "./advent_of_code_day_1.input"): Unit =
  val numbers = linesToInput(filename)
  val slidings = numbers.sliding(3,1)
  val summedSlidings = slidings.map(window => window.sum).toList
  val states = depthsToState(summedSlidings)

  val numsIncreased: Int = states.collect{
    case state if state == State.Increased => state
  }.size

  println(numsIncreased)

