/**
 * Created by nathan on 12/9/16.
 * represents pieces in the game - also represents the board
 */

abstract class Piece {
  val name: String
  val color: String
  val layout: Array[Array[Cell]]

  val usage = Counter()

  lazy val rows: Int = layout.length
  lazy val cols: Int = layout(0).length

  lazy val printFillString: String = List.fill(cols * 2 + 1)(" ").mkString // used by the game to output a blank row when printed side by side with other pieces

  // point value is calculated once and used to know the point of a piece
  lazy val pointValue: Int = onPositions.length

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

    s.toString.dropRight(1)
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
class Line(val name: String, val color: String, length: Int) extends Piece {
  private val a = Array.fill(length)(true)
  val layout: Array[Array[Cell]] = Piece.getLayout(color, Array(a))
}

// generic box class - let's you create boxes of different sizes
// size is not marked as val because it is not a field we retain
// although size does matter
class Box(val name: String, val color: String, size: Int) extends Piece {
  val layout: Array[Array[Cell]] = Piece.getLayout(color, Piece.getBoxTemplateOfSize(size))
}

// generic El class - let's you create L's of different sizes
// size is not marked as val because it is not a field we retain
class El(val name: String, val color: String, size: Int) extends Piece {
  private val a = Piece.getBoxTemplateOfSize(size)

  for {
    i <- 0 to (size - 2)
    j <- 1 until size
  } a(i)(j) = false

  val layout: Array[Array[Cell]] = Piece.getLayout(color, a)
}

object Piece {

  // this is called by board piece constructors to get their layout
  def getLayout(color: String, template: Array[Array[Boolean]]): Array[Array[Cell]] = {
    getLayoutImpl(color, template)
  }

  def getBoardLayout(color: String, size: Int): Array[Array[Cell]] = {
    // this special getLayout is only called to construct the board - so the layout is always going to be
    // everything off - the layout template is going to be the default of Boolean - which is false at every position
    // generated 2D array
    getLayoutImpl(color, Array.ofDim[Boolean](size, size))
  }

  private def getLayoutImpl(color: String, template: Array[Array[Boolean]]): Array[Array[Cell]] = {

    // a layout is an array of Cell objects that conform to a template defined by a 2D array of booleans indicating
    // whether a particular cell is occupied or not.  In this implementation, the color is the same for the entire piece
    // and showUnoccupied indicates whether this cell has an occupied value or not.
    Array.tabulate(template.length, template(0).length) { (i, j) => new Cell(template(i)(j), color) }

  }

  // take a piece, and create a new piece, rotated 90 degrees
  def rotate90(newName: String, pieceToCopy: Piece): Piece = {

    class Rotated(val name: String, val color: String, val layout: Array[Array[Cell]]) extends Piece

    val rotatedLayout = pieceToCopy.layout.transpose.map(_.reverse).map(_.toArray)
    new Rotated(newName, pieceToCopy.color, rotatedLayout)

  }

  def getBoxTemplateOfSize(size: Int): Array[Array[Boolean]] = Array.tabulate(size, size) { (i, j) => true }

}