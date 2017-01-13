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

class Pieces {

  import Pieces._

  // format: OFF
  // map as a convenience for requesting specific named pieces
  private val pieceMap = Map(
    singleton.name       -> singleton,      // 0
    h2line.name          -> h2line,         // 1
    v2line.name          -> v2line,         // 2
    h3line.name          -> h3line,         // 3
    v3line.name          -> v3line,         // 4
    h4line.name          -> h4line,         // 5
    v4line.name          -> v4line,         // 6
    h5line.name          -> h5line,         // 7
    v5line.name          -> v5line,         // 8
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
    val pieceIndex = scala.util.Random.nextInt(pieceDistribution.size)
    pieceMap(pieceDistribution(pieceIndex))
  }

  def getNamedPieces(names: String*): List[Piece] = {
    names.map(name => pieceMap(name)).toList
  }

  def usageString: String = {

    pieceList
      .sortBy(piece => (piece.usage.value, piece.name))
      .map(piece => Game.labelNumberFormat.format(piece.name, piece.usage.value))
      .mkString("\n")
  }

  def printPossiblePieces(): Unit = {
    for (piece <- pieceList) {
      println(piece.name + ": " + piece.pointValue)
      println(piece.show)
    }
    println
  }

}

/*ase class PieceSpecification(
  name:      String,
  pieceType: String, // how could you turn this into
  size:      Int,
  color:     String,
  weight:    Int
)*/

object Pieces {

  val singletonName = "Singleton"
  val h2lineName = "HLine2"
  val v2lineName = "VLine2"
  val h3lineName = "HLine3"
  val v3lineName = "VLine3"
  val h4lineName = "HLine4"
  val v4lineName = "VLine4"
  val h5lineName = "HLine5"
  val v5lineName = "VLine5"
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

  val singleton = new Line(Piece.getLinearGrid(1), Pieces.singletonName, Game.BLACK, 4)

  val h2line = new Line(Piece.getLinearGrid(2), Pieces.h2lineName, Game.BRIGHT_YELLOW, 6)
  val v2line: Piece = Piece.rotate90(Pieces.v2lineName, h2line)

  val h3line = new Line(Piece.getLinearGrid(3), Pieces.h3lineName, Game.YELLOW, 6)
  val v3line: Piece = Piece.rotate90(Pieces.v3lineName, h3line)

  val h4line = new Line(Piece.getLinearGrid(4), Pieces.h4lineName, Game.BRIGHT_RED, 4)
  val v4line: Piece = Piece.rotate90(Pieces.v4lineName, h4line)

  val h5line = new Line(Piece.getLinearGrid(5), Pieces.h5lineName, Game.RED, 4)
  val v5line: Piece = Piece.rotate90(Pieces.v5lineName, h5line)

  val box = new Box(Piece.getBoxGrid(2), Pieces.boxName, Game.GREEN, 12)
  val bigBox = new Box(Piece.getBoxGrid(3), Pieces.bigBoxName, Game.CYAN, 4)

  val lowerLeftEl = new El(Piece.getBoxGrid(2), Pieces.lowerLeftElName, Game.BRIGHT_CYAN, 3)
  val upperLeftEl: Piece = Piece.rotate90(Pieces.upperLeftElName, lowerLeftEl)
  val upperRightEl: Piece = Piece.rotate90(Pieces.upperRightElName, upperLeftEl)
  val lowerRightEl: Piece = Piece.rotate90(Pieces.lowerRightElName, upperRightEl)

  val bigLowerLeftEl = new El(Piece.getBoxGrid(2), Pieces.bigLowerLeftElName, Game.BLUE, 2)
  val bigUpperLeftEl: Piece = Piece.rotate90(Pieces.bigUpperLeftElName, bigLowerLeftEl)
  val bigUpperRightEl: Piece = Piece.rotate90(Pieces.bigUpperRightElName, bigUpperLeftEl)
  val bigLowerRightEl: Piece = Piece.rotate90(Pieces.bigLowerRightElName, bigUpperRightEl)
}
