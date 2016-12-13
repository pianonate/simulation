/**
 * Created by nathan on 12/9/16.
 * represents pieces to try
 */

abstract class Piece {
  val name: String
  val color: Ansi
  val layout: Array[Array[Cell]]

  var usage: Int = 0

  lazy val rows: Int = layout.length
  lazy val cols: Int = layout(0).length

  lazy val printFillString: String = List.fill(cols * 2 + 1)(" ").mkString // used by the game to output a blank row when printed side by side with other pieces

  // point value is calculated once and used to know the point of a piece
  lazy val initialPointValue: Int = onPositions.length

  // the score is the count of every occupied position in a piece
  private def onPositions: Array[Cell] = for {
    i <- layout
    j <- i
    if j.occupied // filter out off positions
  } yield j

  // boards can have their Cell's changed so that the occupied status will change
  // we can use currentPointValue when comparing Board state
  def occupiedCount: Int = onPositions.length

  override def toString: String = {

    val s = new StringBuilder()
    for { row <- layout } {
      s ++= row.map(cellToStringMapFunction).foldRight(" ")((a, b) => a + " " + b) + "\n"
    }

    s.toString
  }

  // when outputting pieces individually, don't output anything for unoccupied cells
  // mimic'd by using a single space
  protected def cellToStringMapFunction(cell: Cell): String = {
    if (cell.occupied)
      cell.toString
    else
      " "
  }

}

// generic line class - let's you create a bunch of different lines
// length is not marked as val because it is not a field we retain
class Line(val name: String, val color: Ansi, length: Int) extends Piece {
  private val a = Array.fill(length)(true)
  val layout: Array[Array[Cell]] = Piece.getLayout(color, Array(a))
}

// generic box class - let's you create boxes of different sizes
// size is not marked as val because it is not a field we retain
// although size does matter
class Box(val name: String, val color: Ansi, size: Int) extends Piece {
  val layout: Array[Array[Cell]] = Piece.getLayout(color, Piece.getBoxTemplateOfSize(size))
}

// generic El class - let's you create L's of different sizes
// size is not marked as val because it is not a field we retain
class El(val name: String, val color: Ansi, size: Int) extends Piece {
  private val a = Piece.getBoxTemplateOfSize(size)

  for {
    i <- 0 to (size - 2)
    j <- 1 until size
  } a(i)(j) = false

  val layout: Array[Array[Cell]] = Piece.getLayout(color, a)
}

/*
 following are what the templates look like for creating lines, boxes, and El's
 if necessary, these are rotated to create all the necessary piece types

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

object Piece {

  private val singleton = new Line("Singleton", Ansi.Black, 1)

  private val h2line = new Line("HorizontalLine2", Ansi.BrightYellow, 2)
  private val v2line = Piece.rotate90("VerticalLine2", h2line)

  private val h3line = new Line("HorizontalLine3", Ansi.Yellow, 3)
  private val v3line = Piece.rotate90("VerticalLine3", h3line)

  private val h4line = new Line("HorizontalLine4", Ansi.BrightRed, 4)
  private val v4line = Piece.rotate90("VerticalLine4", h4line)

  private val h5line = new Line("HorizontalLine5", Ansi.Red, 5)
  private val v5line = Piece.rotate90("VerticalLine5", h5line)

  private val box = new Box("Box", Ansi.Green, 2)
  private val bigbox = new Box("BigBox", Ansi.Cyan, 3)

  private val lowerLeftEl = new El("LowerLeftEl", Ansi.BrightCyan, 2)
  private val upperLeftEl = Piece.rotate90("UpperLeftEl", lowerLeftEl)
  private val upperRightEl = Piece.rotate90("UpperRightEl", upperLeftEl)
  private val lowerRightEl = Piece.rotate90("LowerRightEl", upperRightEl)

  private val bigLowerLeftEl = new El("BigLowerLeftEl", Ansi.Blue, 3)
  private val bigUpperLeftEl = Piece.rotate90("BigUpperLeftEl", bigLowerLeftEl)
  private val bigUpperRightEl = Piece.rotate90("BigUpperRightEl", bigUpperLeftEl)
  private val bigLowerRightEl = Piece.rotate90("BigLowerRightEl", bigUpperRightEl)

  val pieces: List[Piece] = List[Piece](
    singleton, // 0
    h2line, // 1
    v2line, // 2
    h3line, // 3
    v3line, // 4
    h4line, // 5
    v4line, // 6
    h5line, // 7
    v5line, // 8
    box, // 9
    bigbox, // 10
    lowerLeftEl, // 11
    upperLeftEl, // 12
    upperRightEl, // 13
    lowerRightEl, // 14
    bigLowerLeftEl, // 15
    bigUpperLeftEl, // 16
    bigUpperRightEl, // 17
    bigLowerRightEl // 18

  )

  def randomPiece: Piece = {
    // add a random piece to the board and print it out
    val pieceIndex = scala.util.Random.nextInt(Piece.pieces.size)
    pieces(pieceIndex)

  }

  // this is called by board piece constructors to get their layout
  def getLayout(color: Ansi, template: Array[Array[Boolean]]): Array[Array[Cell]] = {
    getLayoutImpl(color, template)
  }

  def getBoardLayout(size: Int, color: Ansi): Array[Array[Cell]] = {
    // this special getLayout is only called to construct the board - so the layout is always going to be
    // everything off - the layout template is going to be the default of Boolean - which is false at every position
    // generated 2D array
    getLayoutImpl(color, Array.ofDim[Boolean](size, size))
  }

  private def getLayoutImpl(color: Ansi, template: Array[Array[Boolean]]): Array[Array[Cell]] = {

    // a layout is an array of Cell objects that conform to a template defined by a 2D array of booleans indicating
    // whether a particular cell is occupied or not.  In this implementation, the color is the same for the entire piece
    // and showUnoccupied indicates whether this cell has an occupied value or not.
    Array.tabulate(template.length, template(0).length) { (i, j) => new Cell(template(i)(j), color) }

  }

  // take a piece, and create a new piece, rotated 90 degrees
  def rotate90(newName: String, pieceToCopy: Piece): Piece = {

    class Rotated(val name: String, val color: Ansi, val layout: Array[Array[Cell]]) extends Piece
    val rotatedLayout = pieceToCopy.layout.transpose.map(_.reverse).map(_.toArray)
    new Rotated(newName, pieceToCopy.color, rotatedLayout)

  }

  def getBoxTemplateOfSize(size: Int): Array[Array[Boolean]] = Array.tabulate(size, size) { (i, j) => true }

}