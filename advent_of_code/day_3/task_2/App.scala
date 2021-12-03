
import scala.io.Source
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import Integer.parseInt


def testInput = "./test.input"
def taskInput = "./task.input"

type Input = List[List[Char]]

def inputToList(filename: String): Input =
  Source.fromFile(filename)
    .getLines.toList
    .filter(_.nonEmpty)
    .map(_.toList)

case object BitCounter:
  def empty: BitCounter = BitCounter(0, 0)


case class BitCounter(zeros: Int, ones: Int):
  private def neg(i: Int)  = if i == 1 then 0 else 1
  def mostCommon: Int      = if ones >= zeros then 1 else 0
  def leastCommon: Int     = neg(mostCommon)
  def plusOne:  BitCounter = this.copy(zeros, ones + 1)
  def plusZero: BitCounter = this.copy(zeros + 1, ones)


class Counter(val bitsetSize: Int):
  private val underlying = new ArrayBuffer[BitCounter]
  private def prettyPrint(int: Int): Char = if int == 1 then '1' else '0'

  0 until bitsetSize foreach { index =>
    underlying append BitCounter.empty
  }

  def get(index: Int): BitCounter =
    underlying(index)

  def getMostCommonAt(index: Int): Char = prettyPrint {
    underlying(index).mostCommon
  }

  def getLeastCommonAt(index: Int): Char = prettyPrint {
    underlying(index).leastCommon
  }

  def zero(pos: Int): Unit =
    underlying.update(pos, underlying(pos).plusZero)

  def one(pos: Int): Unit =
    underlying.update(pos, underlying(pos).plusOne)

  def gamma: String =
    underlying.map(_.mostCommon).mkString

  def epsilon: String =
    underlying.map(_.leastCommon).mkString


def getBitsetSize(input: Input): Int =
  input.headOption.map(_.size).getOrElse(0)

// The most common bit of each position
def calcTheLeastAndMostCommonBits(bitsetSize: Int)(input: Input): Counter =
  val counter = Counter(bitsetSize)

  @tailrec def iter(inputsLeft: Input): Unit =
    inputsLeft match
      case Nil        => ()
      case bits :: xs =>
        for pos <- 0 until bits.size do
          if bits(pos) == '0'
            then counter.zero(pos)
            else counter.one(pos)

        iter(xs)
  iter(input)
  counter


def calcTheRatings(input: Input): Option[(Int, Int)] =
  var filteredByCommon = input
  var filteredByRare   = input
  val bitsetSize       = getBitsetSize(input)

  for pos <- 0 until bitsetSize do
    val commonCounter  = calcTheLeastAndMostCommonBits(bitsetSize)(filteredByCommon)
    val commonBitAtPos = commonCounter.getMostCommonAt(pos)

    if filteredByCommon.length > 1 then
      filteredByCommon = filteredByCommon filter { elem =>
        elem(pos) == commonBitAtPos
      }

  for pos <- 0 until bitsetSize do
    val rareCounter  = calcTheLeastAndMostCommonBits(bitsetSize)(filteredByRare)
    val rareBitAtPos = rareCounter.getLeastCommonAt(pos)

    if filteredByRare.length > 1 then
      filteredByRare = filteredByRare filter { elem =>
        elem(pos) == rareBitAtPos
      }

  val commonOpt = filteredByCommon.headOption.map(_.mkString)
  val rareOpt   = filteredByRare.headOption.map(_.mkString)

  for common <- commonOpt
      rare   <- rareOpt
   yield
      (parseInt(common, 2), parseInt(rare, 2))



def main(args: Array[String]): Unit =
  calcTheRatings compose inputToList apply taskInput foreach { (c, r) =>
    println(c * r)
  }

