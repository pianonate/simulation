/**
 * Created by rhialtotm on 1/24/17.
 * implicits are fun
 */

import scala.collection.mutable.ListBuffer
import StringFormats._
import Implicits._
import scala.language.implicitConversions

object Implicits {

  implicit def array2EnhancedArray(a: Array[Int]): EnhancedArray = new EnhancedArray(a)

  implicit def counter2CounterFormats(counter: Counter): CounterFormats = new CounterFormats(counter)

  implicit def double2DoubleFormats(d: Double): DoubleFormats = new DoubleFormats(d)

  implicit def in2IntFormats(i: Int): IntFormats = new IntFormats(i)

  implicit def float2FloatFormats(f: Float): FloatFormats = new FloatFormats(f)

  implicit def gameTimer2GameTimerFormats(gameTimer: GameTimer): GameTimerFormats = new GameTimerFormats(gameTimer)

  implicit def pieceArray2PieceArrayFormats(pieces: Array[Piece]): PieceArrayFormats = new PieceArrayFormats(pieces)

  implicit def listBuffer2EnhancedListBuffer(buf: ListBuffer[Int]): EnhancedListBuffer = new EnhancedListBuffer(buf)

  implicit def arrayString2EnhancedArrayString(xs: Array[String]): EnhancedArrayString = new EnhancedArrayString(xs)

  implicit def long2LongFormats(l: Long): LongFormats = new LongFormats(l)

  implicit def string2StringFormats(s: String): StringFormats = new StringFormats(s)

  implicit def string2JSONFormats(s: String): JSONFormats = new JSONFormats(s)

}

class CounterFormats(val counter: Counter) {
  private val current: Int = counter.value

  def label: String = current.label
  def label(length: Int): String = current.label(length)
  def shortLabel: String = current.shortLabel
  def scoreLabel: String = current.scoreLabel
}

class DoubleFormats(val d: Double) {
  def label(length: Int): String = ("%1." + length + "f").format(d)

  def label(integerPartLength: Int, fractionalPartLength: Int): String = {
    ("%" + (integerPartLength + 1 + fractionalPartLength) + "." + fractionalPartLength + "f").format(d)
  }

  def weightLabel: String = label(StringFormats.weightFormatLength)

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

class EnhancedArrayString(val xs: Array[String]) {
  def spreadHorizontal(startAt: Int = 0, bracketWith: String = "", separator: String = " "): String =
    xs.map(_.split("\n"))
      .transpose
      .map(each => " ".repeat(startAt) + bracketWith + each.mkString(separator).init + (if (bracketWith.length > 0) " " else "") + bracketWith)
      .mkString("\n")
}

class IntFormats(val i: Int) {
  def firstSecondThirdLabel: String = i match {
    case 1 => "1st"
    case 2 => "2nd"
    case 3 => "3rd"
  }

  def greenPerSecondLabel: String = GREEN + perSecondLabel + SANE
  def label: String = numberFormat.format(i)
  def label(length: Int): String = ("%," + length.toString + "d").format(i)
  def optFactorLabel: String = optFactorResultFormat.format(i)
  def perSecondLabel: String = label + "/s"
  def rightAligned(length: Int): String = ("%" + length + "d").format(i)
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

class PieceArrayFormats(val pieceList: Array[Piece]) {

  def permutationPiecesHeader(index: Int, gamePieces: GamePieces): String = {

    val maxPieceWidth = gamePieces.widestPiece * 2 - 1 // * 2 because there are spaces between each box

    val numPieces = Game.numPiecesInRound

    val header = "Permutation".appendColon + (index + 1) + (if (index == 0) " (game selected)" else "")
    val headerBuffer = (maxPieceWidth * numPieces) + (numPieces * 3) - (header.length + 2)
    val paddedHeader = header + " ".repeat(headerBuffer) + "\n"

    val pieceStrings = paddedHeader +
      pieceList.map(
        piece => {
          val pieceBuffer = maxPieceWidth - (piece.cols * 2 - 1)

          piece.show.split("\n").map(each => each + " ".repeat(pieceBuffer)).mkString("\n") +
            (piece.rows until gamePieces.tallestPiece).map(_ => "\n" + " ".repeat(maxPieceWidth + 2)).mkString

        }
      ).spreadHorizontal()

    pieceStrings

  }
}

class LongFormats(val l: Long) {
  def msLabel(length: Int): String = " in " + ("%," + length.toString + "d").format(l) + "ms"
}

class JSONFormats(val s: String) {

  // json name value pair
  def jsonNameValuePair(value: Any): String = {
    jsonNameValuePair(value, delimited = true)
  }

  def jsonNameValuePairLast(value: Any): String = {
    jsonNameValuePair(value, delimited = false)
  }

  private def jsonNameValuePair(value: Any, delimited: Boolean) = {
    "\"" + s + "\":" + value.toString + (if (delimited) JSONFormats.delimiter else "")
  }

}

class StringFormats(val s: String) {
  private def getHeaderString(color: String): String = {

    val padLength = (headerWidth - (s.length + 2)) / 2
    val pad1 = "-" * padLength
    val pad2 = "-" * (headerWidth - (padLength + s.length + 2))

    "\n" + color + pad1 + " " + s + " " + pad2 + SANE
  }

  def appendColon: String = s + ": "
  def curlyBraces: String = "{" + s + "}"
  def doubleQuote: String = "\"" + s + "\""
  def green: String = GREEN + s + SANE
  def greenHeader: String = getHeaderString(GREEN)
  // def greenDigits:String = coloredDigitsLabel(GREEN)
  def header: String = getHeaderString(CYAN)
  def label: String = labelFormat.format(s)
  def optFactorLabel: String = optFactorLabelFormat.format(s)
  def leftAlignedPadded(length: Int): String = ("%-" + length.toString + "s").format(s)
  def parens: String = " (" + s + ")"
  def plural(i: Int): String = s + (if (i == 1) "" else "s")
  def redHeader: String = getHeaderString(RED)
  def repeat(length: Int): String = s * length
  def rightAlignedPadded(length: Int): String = ("%" + length.toString + "s").format(s)
  def squareBracket: String = "[" + s + "]"
  // def yellowDigits:String = coloredDigitsLabel(YELLOW)
  def underline: String = UNDERLINE + s + SANE
  def yellow: String = YELLOW + s + SANE

  def splice(splicee: Array[String]): String = {
    s.split("\n").zip(splicee).map { case (a, b) => a + b }.mkString("\n")
  }

  def wrap(width: Int, height: Int, color: String): String = {

    val first = s.split(" ").foldLeft(Array(""))((out, in) => {
      if ((out.last + " " + in).trim.length > width) out :+ in
      else out.updated(out.length - 1, out.last + " " + in)
    }).mkString("\n").trim

    val firstArray = first.split("\n")

    val second = (firstArray.length until height).map(_ => " ".repeat(width)).toArray

    val wrapped = firstArray.map(each => color + each.leftAlignedPadded(width) + SANE) ++ second
    wrapped.mkString("\n")

  }
}

object JSONFormats {
  val delimiter = ","
}

object StringFormats {

  val labelFormatLength = 21
  val numberFormatLength = 11
  val headerWidth: Int = labelFormatLength + numberFormatLength + 15

  val labelFormat: String = "%-" + labelFormatLength.toString + "s: "
  val optFactorLabelFormat = " %s: "
  val optFactorResultFormat = "%,3d"

  val numberFormat: String = "%," + numberFormatLength.toString + "d"
  val floatFormat: String = "%" + (numberFormatLength + 2).toString + ".1f"

  val weightFormatLength = 4

  val numberFormatShort = "%,d"
  val elapsedFormat: String = "%" + (numberFormatLength + 2).toString + "s"
  val elapsedFormatMs: String = "%" + (numberFormatLength + 7).toString + "s"

  val percentFormat = " %2.2f"
  val skippedSimulationPercentFormat = "     %2.0f"

  val VERTICAL_LINE = "\u2503"
  val HORIZONTAL_LINE = "\u2501"
  val CROSS_PIECE = "\u254B"
  val VERTICAL_AND_LEFT = "\u252B"

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

  val CLEAR_SCREEN: String = ESCAPE + "[2J" + ESCAPE + "[0;0H"

}
