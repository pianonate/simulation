/**
 * Created by nathan on 12/18/16.
 * a game needs a new collection of pieces on each run because the pieces maintain their own usage per run
 * todo if you don't like the sort of pieces at end of game sort, make this thing ordered and provide your own compare
 *
 * update with kevin's distribution suggestion - these relative weights
 * and then select from a random instance of each type:
 * El 3
 * Box 3
 * Line2 3
 * Line3 3
 * Line4 2
 * Line5 2
 * BigEl 2
 * BigBox 1
 * Singleton 1
 *
 *
 * Practically - we can do this by expanding
 *
 * El         3 * 4 = 12	Each El is 3
 * Box        3 * 4 =	12	Each Box is 12!
 * Line2      3	* 4 = 12	Each Line2 is 6
 * Line3      3	* 4 = 12  Each Line3 is 6
 * Line4      2	* 4 = 8   Each Line4 is 4
 * Line5      2	* 4 = 8   Each Line5 is 4
 * BigEl      2	* 4 = 8   Each BigEl is 2
 * BigBox     1	* 4 = 4   Each BigBox is 4
 * Singleton  1	* 4 = 4   Each Singleton is 4
 */
import Implicits._

import scala.util.Random

/*
 following are what the templates look like for creating lines, boxes, and El's
 if necessary, these are then rotated to create all the necessary piece types

 Singleton
 ■

 H2Line
 ■ ■

 H3Line
 ■ ■ ■

 H4Line
 ■ ■ ■ ■

 H5Line
 ■ ■ ■ ■ ■

 Box
 ■ ■
 ■ ■

 BigBox
 ■ ■ ■
 ■ ■ ■
 ■ ■ ■

 Lower Left L:
 ■
 ■ ■

 Big Lower Left L
 ■
 ■
 ■ ■ ■

 */

class GamePieces(val seed: Int, boardSizeInfo: BoardSizeInfo) extends Output {

  private[this] val singletonName = "Singleton"
  private[this] val h2LineName = "HLine2"
  private[this] val v2LineName = "VLine2"
  private[this] val h3LineName = "HLine3"
  private[this] val v3LineName = "VLine3"
  private[this] val h4LineName = "HLine4"
  private[this] val v4LineName = "VLine4"
  private[this] val h5LineName = "HLine5"
  private[this] val v5LineName = "VLine5"
  private[this] val boxName = "Box"
  private[this] val bigBoxName = "BigBox"
  private[this] val lowerLeftElName = "LowerLL"
  private[this] val upperLeftElName = "UpperLL"
  private[this] val upperRightElName = "UpperRL"
  private[this] val lowerRightElName = "LowerRL"
  private[this] val bigLowerLeftElName = "BigLowerLL"
  private[this] val bigUpperLeftElName = "BigUpperLL"
  private[this] val bigUpperRightElName = "BigUpperRL"
  private[this] val bigLowerRightElName = "BigLowerRL"

  // used for testing otherwise they could just be private
  val singleton = Line(Piece.getLinearGrid(1, boardSizeInfo), singletonName, StringFormats.BLUE, Piece.primeIterator.next, 4)

  val h2Line = Line(Piece.getLinearGrid(2, boardSizeInfo), h2LineName, StringFormats.BRIGHT_YELLOW, Piece.primeIterator.next, 6)
  val v2Line: Piece = Piece.rotate90(v2LineName, h2Line)

  val h3Line = Line(Piece.getLinearGrid(3, boardSizeInfo), h3LineName, StringFormats.YELLOW, Piece.primeIterator.next, 6)
  val v3Line: Piece = Piece.rotate90(v3LineName, h3Line)

  val h4Line = Line(Piece.getLinearGrid(4, boardSizeInfo), h4LineName, StringFormats.BRIGHT_RED, Piece.primeIterator.next, 4)
  val v4Line: Piece = Piece.rotate90(v4LineName, h4Line)

  val h5Line = Line(Piece.getLinearGrid(5, boardSizeInfo), h5LineName, StringFormats.RED, Piece.primeIterator.next, 4)
  val v5Line: Piece = Piece.rotate90(v5LineName, h5Line)

  val box = Box(Piece.getBoxGrid(2, boardSizeInfo), boxName, StringFormats.GREEN, Piece.primeIterator.next, 12)
  val bigBox = Box(Piece.getBoxGrid(3, boardSizeInfo), bigBoxName, StringFormats.CYAN, Piece.primeIterator.next, 4)

  val lowerLeftEl = El(Piece.getElGrid(2, boardSizeInfo), lowerLeftElName, StringFormats.MAGENTA, Piece.primeIterator.next, 3)
  val upperLeftEl: Piece = Piece.rotate90(upperLeftElName, lowerLeftEl)
  val upperRightEl: Piece = Piece.rotate90(upperRightElName, upperLeftEl)
  val lowerRightEl: Piece = Piece.rotate90(lowerRightElName, upperRightEl)

  val bigLowerLeftEl = El(Piece.getElGrid(3, boardSizeInfo), bigLowerLeftElName, StringFormats.BRIGHT_BLUE, Piece.primeIterator.next, 2)
  val bigUpperLeftEl: Piece = Piece.rotate90(bigUpperLeftElName, bigLowerLeftEl)
  val bigUpperRightEl: Piece = Piece.rotate90(bigUpperRightElName, bigUpperLeftEl)
  val bigLowerRightEl: Piece = Piece.rotate90(bigLowerRightElName, bigUpperRightEl)

  // set the piece generator to be seeded by the passed in seed
  private[this] val randomizer: Random = new scala.util.Random(seed)

  // format: OFF
  // map as a convenience for requesting specific named pieces
  private val pieceMap = Map(
    singleton.name       -> singleton,      // 0
    h2Line.name          -> h2Line,         // 1
    v2Line.name          -> v2Line,         // 2
    h3Line.name          -> h3Line,         // 3
    v3Line.name          -> v3Line,         // 4
    h4Line.name          -> h4Line,         // 5
    v4Line.name          -> v4Line,         // 6
    h5Line.name          -> h5Line,         // 7
    v5Line.name          -> v5Line,         // 8
    box.name             -> box,            // 9
    bigBox.name          -> bigBox,         // 10
    lowerLeftEl.name     -> lowerLeftEl,    // 11
    upperLeftEl.name     -> upperLeftEl,    // 12
    upperRightEl.name    -> upperRightEl,   // 13
    lowerRightEl.name    -> lowerRightEl,   // 14
    bigLowerLeftEl.name  -> bigLowerLeftEl, // 15
    bigUpperLeftEl.name  -> bigUpperLeftEl, // 16
    bigUpperRightEl.name -> bigUpperRightEl,// 17
    bigLowerRightEl.name -> bigLowerRightEl // 18
  )

  // format: ON

  val pieceList: List[Piece] = pieceMap.values.toList

  private val pieceDistribution = pieceList.flatMap(piece => List.fill(piece.weight)(piece.name))

  def getRandomPiece: Piece = {
    // add a random piece to the board and print it out
    val pieceIndex = randomizer.nextInt(pieceDistribution.size)
    pieceMap(pieceDistribution(pieceIndex))
  }

  def usageString: String = {
    pieceList
      .sortBy(piece => (piece.usage.value, piece.name))
      .map(piece => piece.name.label + piece.usage.label)
      .mkString("\n")
  }

  def printPossiblePieces(context: Context): Unit = {

    context.logger.info("printing possible pieces")

    for (piece <- pieceList.sortBy(_.pointValue)) {
      println(piece.name.appendColon + piece.pointValue)
      println(piece.show)
      println
    }
  }

  // used to find the color index for a particular location in the board colorGrid
  // the color used on the board will have derived from one of these pieces.
  // provide an integer index that we then pack into a BigInt representing the colored state of a given board
  // val colorIndexMap = pieceList.zipWithIndex.map(tup => (tup._1.color, tup._2 + 1)).toMap
  val colorIndexMap: Map[String, Int] = pieceList.map(_.color).toSet.zipWithIndex.map(tup => (tup._1, tup._2 + 1)).toMap
  val tallestPiece: Int = pieceList.map(_.rows).max
  val widestPiece: Int = pieceList.map(_.cols).max
  val longestNameLength: Int = pieceList.map(_.name.length).max

}
