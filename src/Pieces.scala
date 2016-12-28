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

  private val singleton = new Line("Singleton", GameUtil.BLACK, 1, 4)

  private val h2line = new Line("HLine2", GameUtil.BRIGHT_YELLOW, 2, 6)
  private val v2line = Piece.rotate90("VLine2", h2line)

  private val h3line = new Line("HLine3", GameUtil.YELLOW, 3, 6)
  private val v3line = Piece.rotate90("VLine3", h3line)

  private val h4line = new Line("HLine4", GameUtil.BRIGHT_RED, 4, 4)
  private val v4line = Piece.rotate90("VLine4", h4line)

  private val h5line = new Line("HLine5", GameUtil.RED, 5, 4)
  private val v5line = Piece.rotate90("VLine5", h5line)

  private val box = new Box("Box", GameUtil.GREEN, 2, 12)
  private val bigBox = new Box("BigBox", GameUtil.CYAN, 3, 4)

  private val lowerLeftEl = new El("LowerLL", GameUtil.BRIGHT_CYAN, 2, 3)
  private val upperLeftEl = Piece.rotate90("UpperLL", lowerLeftEl)
  private val upperRightEl = Piece.rotate90("UpperRL", upperLeftEl)
  private val lowerRightEl = Piece.rotate90("LowerRL", upperRightEl)

  private val bigLowerLeftEl = new El("BigLowerLL", GameUtil.BLUE, 3, 2)
  private val bigUpperLeftEl = Piece.rotate90("BigUpperLL", bigLowerLeftEl)
  private val bigUpperRightEl = Piece.rotate90("BigUpperRL", bigUpperLeftEl)
  private val bigLowerRightEl = Piece.rotate90("BigLowerRL", bigUpperRightEl)

  // map as a convenience for requesting specific named pieces
  private val pieceMap = Map(
    singleton.name -> singleton, // 0
    h2line.name -> h2line, // 1
    v2line.name -> v2line, // 2
    h3line.name -> h3line, // 3
    v3line.name -> v3line, // 4
    h4line.name -> h4line, // 5
    v4line.name -> v4line, // 6
    h5line.name -> h5line, // 7
    v5line.name -> v5line, // 8
    box.name -> box, // 9
    bigBox.name -> bigBox, // 10
    lowerLeftEl.name -> lowerLeftEl, // 11
    upperLeftEl.name -> upperLeftEl, // 12
    upperRightEl.name -> upperRightEl, // 13
    lowerRightEl.name -> lowerRightEl, // 14
    bigLowerLeftEl.name -> bigLowerLeftEl, // 15
    bigUpperLeftEl.name -> bigUpperLeftEl, // 16
    bigUpperRightEl.name -> bigUpperRightEl, // 17
    bigLowerRightEl.name -> bigLowerRightEl // 18

  )

  val pieceList: List[Piece] = pieceMap.values.toList

  private val pieceDistribution = pieceList.flatMap(piece => List.fill(piece.weight)(piece.name))

  def getRandomPiece: Piece = {
    // add a random piece to the board and print it out
    val pieceIndex = scala.util.Random.nextInt(pieceDistribution.size)
    pieceMap(pieceDistribution(pieceIndex))
  }

  def getNamedPiece(name: String): Piece = pieceMap(name)

  def getNamedPieces(names: String*): List[Piece] = {
    names.map(name => pieceMap(name)).toList
  }

  def usageString: String = {

    pieceList
      .sortBy(piece => (piece.usage.value, piece.name))
      .map(piece => GameUtil.labelNumberFormat.format(piece.name, piece.usage.value))
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
