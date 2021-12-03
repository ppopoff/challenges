
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
  def mostCommon: Int      = if ones > zeros then 1 else 0
  def leastCommon: Int     = if ones < zeros then 1 else 0
  def plusOne:  BitCounter = this.copy(zeros, ones + 1)
  def plusZero: BitCounter = this.copy(zeros + 1, ones)


class Counter(private val bitsetSize: Int):
  private val underlying = new ArrayBuffer[BitCounter]

  0 until bitsetSize foreach { index =>
    underlying append BitCounter.empty
  }

  def zero(pos: Int): Unit =
    underlying.update(pos, underlying(pos).plusZero)

  def one(pos: Int): Unit =
    underlying.update(pos, underlying(pos).plusOne)

  def gamma: Int =
    parseInt(underlying.map(_.mostCommon).mkString, 2)

  def epsilon: Int =
    parseInt(underlying.map(_.leastCommon).mkString, 2)


def calcProduct(input: Input): Int =
  val counter = Counter (
    bitsetSize =
      input.headOption.map(_.size).getOrElse(0)
  )

  @tailrec def iter(inputsLeft: Input): Int = inputsLeft match
    case Nil        => counter.gamma * counter.epsilon
    case bits :: xs =>
      for pos <- 0 until bits.size do
        if bits(pos) == '0'
          then counter.zero(pos)
          else counter.one(pos)
      iter(xs)
    iter(input)


def main(args: Array[String]): Unit = println {
  calcProduct compose inputToList apply taskInput
}

