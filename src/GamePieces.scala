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

class GamePieces(seed:Int = 0) {

  val randomizer: Random = if (seed > 0) new scala.util.Random(seed) else new scala.util.Random()

  import GamePieces._

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

  def getNamedPieces(names: String*): List[Piece] = {
    names.map(name => pieceMap(name)).toList
  }

  def usageString: String = {

    pieceList
      .sortBy(piece => (piece.usage.value, piece.name))
      .map(piece => piece.name.label + piece.usage.label)
      .mkString("\n")
  }

  def printPossiblePieces(): Unit = {
    for (piece <- pieceList) {
      println(piece.name.addColon + piece.pointValue)
      println(piece.show(piece.cellShowFunction))
    }
    println
  }

}

object GamePieces {

  val numPiecesInRound:Int = 3

  val singletonName = "Singleton"
  val h2LineName = "HLine2"
  val v2LineName = "VLine2"
  val h3LineName = "HLine3"
  val v3LineName = "VLine3"
  val h4LineName = "HLine4"
  val v4LineName = "VLine4"
  val h5LineName = "HLine5"
  val v5LineName = "VLine5"
  val boxName = "Box"
  val bigBoxName = "BigBox"
  val lowerLeftElName = "LowerLL"
  val upperLeftElName = "UpperLL"
  val upperRightElName = "UpperRL"
  val lowerRightElName = "LowerRL"
  val bigLowerLeftElName = "BigLowerLL"
  val bigUpperLeftElName = "BigUpperLL"
  val bigUpperRightElName = "BigUpperRL"
  val bigLowerRightElName = "BigLowerRL"

  def singleton = Line(Piece.getLinearGrid(1), GamePieces.singletonName, StringFormats.BLUE, 4)

  def h2Line = Line(Piece.getLinearGrid(2), GamePieces.h2LineName, StringFormats.BRIGHT_YELLOW, 6)
  def v2Line: Piece = Piece.rotate90(GamePieces.v2LineName, h2Line)

  def h3Line = Line(Piece.getLinearGrid(3), GamePieces.h3LineName, StringFormats.YELLOW, 6)
  def v3Line: Piece = Piece.rotate90(GamePieces.v3LineName, h3Line)

  def h4Line = Line(Piece.getLinearGrid(4), GamePieces.h4LineName, StringFormats.BRIGHT_RED, 4)
  def v4Line: Piece = Piece.rotate90(GamePieces.v4LineName, h4Line)

  def h5Line = Line(Piece.getLinearGrid(5), GamePieces.h5LineName, StringFormats.RED, 4)
  def v5Line: Piece = Piece.rotate90(GamePieces.v5LineName, h5Line)

  def box = Box(Piece.getBoxGrid(2), GamePieces.boxName, StringFormats.GREEN, 12)
  def bigBox = Box(Piece.getBoxGrid(3), GamePieces.bigBoxName, StringFormats.CYAN, 4)

  def lowerLeftEl = El(Piece.getBoxGrid(2), GamePieces.lowerLeftElName, StringFormats.MAGENTA, 3)
  def upperLeftEl: Piece = Piece.rotate90(GamePieces.upperLeftElName, lowerLeftEl)
  def upperRightEl: Piece = Piece.rotate90(GamePieces.upperRightElName, upperLeftEl)
  def lowerRightEl: Piece = Piece.rotate90(GamePieces.lowerRightElName, upperRightEl)

  def bigLowerLeftEl = El(Piece.getBoxGrid(3), GamePieces.bigLowerLeftElName, StringFormats.CYAN, 2)
  def bigUpperLeftEl: Piece = Piece.rotate90(GamePieces.bigUpperLeftElName, bigLowerLeftEl)
  def bigUpperRightEl: Piece = Piece.rotate90(GamePieces.bigUpperRightElName, bigUpperLeftEl)
  def bigLowerRightEl: Piece = Piece.rotate90(GamePieces.bigLowerRightElName, bigUpperRightEl)

  private val gamePieces = new GamePieces
  // used for outputting a set of three pieces
  val tallestPiece:Int = gamePieces.pieceList.map(_.rows).max
  val widestPiece:Int = gamePieces.pieceList.map(_.cols).max
  val longestNameLength:Int = gamePieces.pieceList.map(_.name.length).max

}
