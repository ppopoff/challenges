
import scala.io.Source

trait FileTools:
  final case class FileName(val filename: String)

  // paths to test and task input files
  val testFile = FileName("./test.input")
  val taskFile = FileName("./task.input")

  def flines(f: FileName): List[String] =
    Source.fromFile(f.filename, "UTF-8")
      .getLines.toList
      .filter(_.nonEmpty)

  def flinesSplitBy(splitRegex: String)(fname: FileName): List[List[String]] =
    flines(fname).map { line =>
      line.split(splitRegex).map(_.trim()).toList
    }

end FileTools


trait IntTools:
  def binToInt(binaryString: String): Int =
    Integer.parseInt(binaryString, 2)

  def hexToInt(hexString: String): Int =
    Integer.parseInt(hexString, 16)

  def intToBinStr(int: Int): String =
    Integer.toBinaryString(int)

  def intToHexStr(int: Int): String =
    Integer.toHexString(int)

end IntTools


trait CollectionTools:

  extension [T] (list: List[T])
    /*
     *  val a = List(1,2,3,4,5,6,7,8,9,10,11,12)
     *  a.windowedBy(3)
     *  List(List(1,2,3), List(4,5,6), List(7,8,9), List(10,11,12))
     */
    def windowedBy(windowSize: Int): List[List[T]] =
      list.sliding(windowSize, windowSize).toList

    def howFrequent: Map[T, Int] =
      list.groupMapReduce(identity)(_ => 1)(_ + _)

end CollectionTools



//trait BoardTools:
//  object Board:
//    def square [T] (size: Int) = new Board [T] (size, size)
//    def of [T] (xsize: Int, y: size: Int) = new Board [T] (xsize, ysize)
//
//  class Board [T] (xsize: Int, ysize: Int) {
//    private val state: ArrayBuffer[ArrayBuffer[]]
//  }

// Matrix transpone
//  private def transponed(): List[List[Cell]] =
//    val transponed = for (col <- 0 until boardSize)
//      yield board.state.map(row => row.apply(col))
//    transponed.toList

/**
 * The most used tools
 */
object Tools extends FileTools with IntTools with CollectionTools