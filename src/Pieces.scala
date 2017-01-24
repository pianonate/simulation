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

class Pieces(seed:Int = 0) {

  val randomizer = if (seed > 0) new scala.util.Random(seed) else new scala.util.Random()


  import Pieces._

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

  // used for outputting a set of three pieces
  val tallestPiece:Int = pieceList.map(_.rows).max

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
      .map(piece => Game.labelNumberFormat.format(piece.name, piece.usage.value))
      .mkString("\n")
  }

  def printPossiblePieces(): Unit = {
    for (piece <- pieceList) {
      println(piece.name + ": " + piece.pointValue)
      println(piece.show(piece.cellShowFunction))
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

  val singleton = new Line(Piece.getLinearGrid(1), Pieces.singletonName, Game.BLACK, 4)

  val h2Line = new Line(Piece.getLinearGrid(2), Pieces.h2LineName, Game.BRIGHT_YELLOW, 6)
  val v2Line: Piece = Piece.rotate90(Pieces.v2LineName, h2Line)

  val h3Line = new Line(Piece.getLinearGrid(3), Pieces.h3LineName, Game.YELLOW, 6)
  val v3Line: Piece = Piece.rotate90(Pieces.v3LineName, h3Line)

  val h4Line = new Line(Piece.getLinearGrid(4), Pieces.h4LineName, Game.BRIGHT_RED, 4)
  val v4Line: Piece = Piece.rotate90(Pieces.v4LineName, h4Line)

  val h5Line = new Line(Piece.getLinearGrid(5), Pieces.h5LineName, Game.RED, 4)
  val v5Line: Piece = Piece.rotate90(Pieces.v5LineName, h5Line)

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
