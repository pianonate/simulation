/**
 * Created by nathan mccoy on 1/24/17.
 * implicits are fun
 */

import scala.collection.mutable.ListBuffer
import StringFormats._
import Implicits._
import scala.language.implicitConversions

object Implicits {

  implicit def array2EnhancedArray(a: Array[Int]): EnhancedArray = new EnhancedArray(a)

  implicit def counter2CounterFormats(counter: Counter): CounterFormats = new CounterFormats(counter)

  implicit def double2DoubleFormats(d:Double): DoubleFormats = new DoubleFormats(d)

  implicit def in2IntFormats(i: Int): IntFormats = new IntFormats(i)

  implicit def float2FloatFormats(f: Float): FloatFormats = new FloatFormats(f)

  implicit def gameTimer2GameTimerFormats(gameTimer: GameTimer): GameTimerFormats = new GameTimerFormats(gameTimer)

  implicit def pieceList2PieceListFormats(pieces: List[Piece]): PieceListFormats = new PieceListFormats(pieces)

  implicit def listBuffer2EnhancedListBuffer(buf: ListBuffer[Int]): EnhancedListBuffer = new EnhancedListBuffer(buf)

  implicit def listString2EnhancedListString(xs:List[String]): EnhancedListString = new EnhancedListString(xs)

  implicit def long2LongFormats(l: Long): LongFormats = new LongFormats(l)

  implicit def string2StringFormats(s: String): StringFormats = new StringFormats(s)

}

class CounterFormats(val counter: Counter) {
  def label: String = counter.value.label
  def shortLabel: String = counter.value.shortLabel
  def boardScoreLabel: String = counter.value.greenLabel
  def scoreLabel: String = counter.value.scoreLabel
}

class DoubleFormats(val d:Double) {
  def weightLabel:String = weightFormat.format(d)
}

class EnhancedListBuffer(val xs: ListBuffer[Int]) {
  // okay - i fucking love implicits now
  def avg: Double = {
    val (sum, length) = xs.foldLeft((0l, 0))({ case ((s, l), x) => (x + s, 1 + l) })
    val result = sum / length
    result
  }
}

class EnhancedArray(val a: Array[Int]) {
  def avg: Double = {
    val (sum, length) = a.foldLeft((0l, 0))({ case ((s, l), x) => (x + s, 1 + l) })
    val result = sum / length
    result
  }
}

class EnhancedListString(val xs:List[String]) {
  def spreadHorizontal(prefixLength:Int = 0, separator:String = " "):String =
    xs.map(_.split("\n"))
    .transpose
      .map(each => " ".repeat(prefixLength) + each.mkString(separator))
      .mkString("\n")
}

class IntFormats(val i: Int) {
  def firstSecondThirdLabel: String = i match {
    case 1 => "1st"
    case 2 => "2nd"
    case 3 => "3rd"
    case _ => "not supported"
  }
  def greenLabel: String = GREEN + optFactorLabel + SANE
  def greenPerSecondLabel: String = GREEN + perSecondLabel + SANE
  def indexLabel:String = (i + 1) + ": "
  def label: String = numberFormat.format(i)
  def label(length: Int): String = ("%," + length.toString + "d").format(i)
  def optFactorLabel: String = optFactorResultFormat.format(i)
  def perSecondLabel: String = label + "/s"
  def redLabel: String = RED + optFactorLabel + SANE
  def scoreLabel: String = GREEN + label + SANE
  def shortLabel: String = numberFormatShort.format(i)
  def yellowPerSecondLabel: String = YELLOW + perSecondLabel + SANE
}

class FloatFormats(val f: Float) {
  def skippedPercentLabel: String = skippedSimulationPercentFormat.format(f * 100) + "%"
  def percentLabel: String = percentFormat.format(f * 100) + "%"
  def label: String = floatFormat.format(f)
}

class GameTimerFormats(val gameTimer: GameTimer) {
  def elapsedLabel: String = elapsedFormat.format(gameTimer.showElapsed)
  def elapsedLabelMs: String = elapsedFormatMs.format(gameTimer.showElapsedMs)
}

class PieceListFormats(val pieceList: List[Piece]) {

  private val permutationHeaderBuffer = GamePieces.widestPiece * 2 - 1

  def label: String = pieceList.map(piece => StringFormats.pieceNameFormat.format(piece.name)).mkString(" ")

  def permutationShowPiecesHeader(index: Int): String = {

    val numPieces = GamePieces.numPiecesInRound

    val header = ("Permutation: " + (index + 1)) + (if (index==1) " (selected by game)" else "")
    val headerBuffer = (permutationHeaderBuffer * numPieces) + (numPieces * numPieces - 1) - header.length
    val paddedHeader = header + " ".repeat(headerBuffer) + "\n"

    val pieceStrings = paddedHeader +
      pieceList.map(
      piece => {
        val pieceBuffer = permutationHeaderBuffer - (piece.cols * 2 - 1)

        piece.show(piece.cellShowFunction).split("\n").map(each => each + " ".repeat(pieceBuffer)).mkString("\n") +
          (piece.rows to GamePieces.tallestPiece).map(_ => "\n" + " ".repeat(permutationHeaderBuffer + 2)).mkString

      }
    ).spreadHorizontal()

    pieceStrings

  }
}

class LongFormats(val l: Long) {
  def msLabel(length: Int): String = " in " + ("%," + length.toString + "d").format(l) + "ms"
}

class StringFormats(val s: String) {
  def label: String = labelFormat.format(s)
  def elapsedLabel: String = elapsedFormat.format(s)
  def repeat(length:Int): String = s * length

  private def getHeaderString(color: String): String = {

    val padLength = (headerWidth - (s.length + 2)) / 2
    val pad1 = "-" * padLength
    val pad2 = "-" * (headerWidth - (padLength + s.length + 2))

    "\n" + color + pad1 + " " + s + " " + pad2 + SANE
  }

  def header: String = getHeaderString(CYAN)
  def greenHeader: String = getHeaderString(GREEN)
  def redHeader: String = getHeaderString(RED)
  def optFactorLabel: String = optFactorLabelFormat.format(s)
  def paddedLabel(length:Int): String = ("%" + length.toString + "s").format(s)
  def parens: String = " (" + s + ")"
  def plural(i: Int): String = s + (if (i == 1) "" else "s")

}

object StringFormats {

  val labelFormatLength = 21
  val numberFormatLength = 11
  val headerWidth: Int = labelFormatLength + numberFormatLength + 15

  val labelFormat: String = "%-" + labelFormatLength.toString + "s: "
  val optFactorLabelFormat = " %s: "
  val optFactorResultFormat = "%,2d"

  val numberFormat: String = "%," + numberFormatLength.toString + "d"
  val floatFormat: String = "%" + (numberFormatLength + 2).toString + ".1f"

  val weightFormatLength = 9
  val weightFormat:String = "%1." + weightFormatLength + "f "

  val numberFormatShort = "%,d"
  val elapsedFormat: String = "%" + (numberFormatLength + 2).toString + "s"
  val elapsedFormatMs: String = "%" + (numberFormatLength + 7).toString + "s"

  val percentFormat = " %2.2f"
  val skippedSimulationPercentFormat = "     %2.0f"

  val pieceNameFormat: String = "%-" + GamePieces.longestNameLength + "s"


  val DOUBLE_VERTICAL_LINE = "\u2551"

  // Color escape sequence strings from:
  // http://www.topmudsites.com/forums/mud-coding/413-java-ansi.html
  val BLACK = "\u001B[30m"
  val RED = "\u001B[31m"
  val GREEN = "\u001B[32m"
  val YELLOW = "\u001B[33m"
  val BLUE = "\u001B[34m"
  val MAGENTA = "\u001B[35m"
  val CYAN = "\u001B[36m"
  val WHITE = "\u001B[37m"

  val SANE = "\u001B[0m"

  val ESCAPE = "\u001B"
  val HIGH_INTENSITY = "\u001B[1m"
  val LOW_INTENSITY = "\u001B[2m"

  val ITALIC = "\u001B[3m"
  val UNDERLINE = "\u001B[4m"
  val BLINK = "\u001B[5m"
  val RAPID_BLINK = "\u001B[6m"
  val REVERSE_VIDEO = "\u001B[7m"
  val INVISIBLE_TEXT = "\u001B[8m"

  val BRIGHT_BLACK = "\u001B[90m"
  val BRIGHT_RED = "\u001B[91m"
  val BRIGHT_GREEN = "\u001B[92m"
  val BRIGHT_YELLOW = "\u001B[93m"
  val BRIGHT_BLUE = "\u001B[94m"
  val BRIGHT_MAGENTA = "\u001B[95m"
  val BRIGHT_CYAN = "\u001B[96m"
  val BRIGHT_WHITE = "\u001B[97m"

  val BACKGROUND_BLACK = "\u001B[40m"
  val BACKGROUND_RED = "\u001B[41m"
  val BACKGROUND_GREEN = "\u001B[42m"
  val BACKGROUND_YELLOW = "\u001B[43m"
  val BACKGROUND_BLUE = "\u001B[44m"
  val BACKGROUND_MAGENTA = "\u001B[45m"
  val BACKGROUND_CYAN = "\u001B[46m"
  val BACKGROUND_WHITE = "\u001B[47m"

}
