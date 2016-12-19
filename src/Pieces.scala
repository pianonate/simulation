/**
 * Created by nathan on 12/18/16.
 * a game needs a new collection of pieces on each run because the pieces maintain their own usage per run
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

  private val singleton = new Line("Singleton", GameUtil.BLACK, 1)

  private val h2line = new Line("HorizontalLine2", GameUtil.BRIGHT_YELLOW, 2)
  private val v2line = Piece.rotate90("VerticalLine2", h2line)

  private val h3line = new Line("HorizontalLine3", GameUtil.YELLOW, 3)
  private val v3line = Piece.rotate90("VerticalLine3", h3line)

  private val h4line = new Line("HorizontalLine4", GameUtil.BRIGHT_RED, 4)
  private val v4line = Piece.rotate90("VerticalLine4", h4line)

  private val h5line = new Line("HorizontalLine5", GameUtil.RED, 5)
  private val v5line = Piece.rotate90("VerticalLine5", h5line)

  private val box = new Box("Box", GameUtil.GREEN, 2)
  private val bigBox = new Box("BigBox", GameUtil.CYAN, 3)

  private val lowerLeftEl = new El("LowerLeftEl", GameUtil.BRIGHT_CYAN, 2)
  private val upperLeftEl = Piece.rotate90("UpperLeftEl", lowerLeftEl)
  private val upperRightEl = Piece.rotate90("UpperRightEl", upperLeftEl)
  private val lowerRightEl = Piece.rotate90("LowerRightEl", upperRightEl)

  private val bigLowerLeftEl = new El("BigLowerLeftEl", GameUtil.BLUE, 3)
  private val bigUpperLeftEl = Piece.rotate90("BigUpperLeftEl", bigLowerLeftEl)
  private val bigUpperRightEl = Piece.rotate90("BigUpperRightEl", bigUpperLeftEl)
  private val bigLowerRightEl = Piece.rotate90("BigLowerRightEl", bigUpperRightEl)

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
    upperLeftEl.name -> upperRightEl, // 13
    lowerRightEl.name -> lowerRightEl, // 14
    bigLowerLeftEl.name -> bigLowerLeftEl, // 15
    bigUpperLeftEl.name -> bigUpperLeftEl, // 16
    bigUpperRightEl.name -> bigUpperRightEl, // 17
    bigLowerRightEl.name -> bigLowerRightEl // 18

  )

  val pieceList: List[Piece] = pieceMap.values.toList

  def getRandomPiece: Piece = {
    // add a random piece to the board and print it out
    val pieceIndex = scala.util.Random.nextInt(pieceList.size)
    pieceList(pieceIndex)

  }

  def getNamedPiece(name: String): Piece = pieceMap(name)

  def getNamedPieces(names: String*): List[Piece] = {
    names.map(name => pieceMap(name)).toList
  }

  override def toString: String = {

    // todo if you don't like this sort, make this thing ordered and provide your own compare
    pieceList
      .sortBy(piece => (piece.usage.head, piece.name))
      .map(piece => GameUtil.labelNumberFormat.format(piece.name, piece.usage.next))
      .mkString("\n")
  }

}
