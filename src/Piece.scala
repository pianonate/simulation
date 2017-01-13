/**
 * Created by nathan on 12/9/16.
 * represents pieces in the game - also represents the board
 *
 *
 */
abstract class Piece {
  val name: String
  val color: String
  val colorGrid: Array[Array[Cell]]
  val grid: OccupancyGrid
  val weight: Int = 0 // defined so subclasses (such as Board) don't have to provide an implementation

  final val usage = Counter()

  final val rows: Int = grid.rows
  final val cols: Int = grid.cols

  // test to see if eliminating the stack frame makes this go faster
  // it does - but i really hate breaking encapsulation - worth it for a
  // 3x improvement in 'isOccupied" - which even still uses about 6% of the code...
  final val occupancyGrid: Array[Array[Boolean]] = grid.getOccupancyGrid


  // todo - pass in fill string as the board doesn't need it
  // for now:  hack!

  final val printFillString: String = if (cols!=Board.BOARD_SIZE) List.fill(cols * 2 + 1)(" ").mkString else "" // used by the game to output a blank row when printed side by side with other pieces

  /**
   * point value is calculated once and used to know the point of a piece
   * it's not just a convenience as we will call pointValue a lot and don't need to
   * recalculate it each time
   */
  final lazy val pointValue: Int = getOccupiedPositions

  /**
   * todo - can you improve the while loop to be more scala like and retain the perf?
   * boards can have their Cell's changed so that the occupied status will change
   * with the original for comprehension the onPositions method took about 10% of the overall
   * execution time.  with the while loop, it takes <1% of execution time
   * colorGrid.flatten.count(_.occupied) takes 5.6% of execution time
   */
  def getOccupiedPositions: Int = {
    /*colorGrid.flatten.count(_.occupied)*/
    var i = 0
    var j = 0
    var count = 0
    while (i < grid.rows) {
      while (j < grid.cols) {
        if (occupancyGrid(i)(j)) {
          count += 1
        }
        j += 1
      }
      j = 0
      i += 1
    }

    count

  }

  override def toString: String = this.name // for the debugger

  def show: String = {

    val s = new StringBuilder()
    for { row <- colorGrid } {
      s ++= row.map(cellShowMapFunction).foldRight(" ")((a, b) => a + " " + b) + "\n"
    }

    // we don't need the final newline as we're sending these things out via println
    s.toString.dropRight(1)
  }

  // when outputting pieces individually, don't output anything for unoccupied cells
  // mimic'd by using a single space
  protected def cellShowMapFunction(cell: Cell): String = {
    if (cell.occupied)
      cell.showForPiece
    else
      " "
  }

}

// generic line class - let's you create a bunch of different lines
// length is not marked as val because it is not a field we retain
class Line(final val grid:OccupancyGrid, final val name: String, final val color: String, final override val weight: Int) extends Piece {
  private val a = Array.fill(grid.cols)(true)
  val colorGrid: Array[Array[Cell]] = Piece.getColorGrid(color, Array(a))
}

// generic box class - let's you create boxes of different sizes
// size is not marked as val because it is not a field we retain
// although size does matter
class Box(final val grid:OccupancyGrid, final val name: String, final val color: String, final override val weight: Int) extends Piece {
  val colorGrid: Array[Array[Cell]] = Piece.getColorGrid(color, Piece.getBoxTemplateOfSize(grid.rows  ))
}

// generic El class - let's you create L's of different sizes
// size is not marked as val because it is not a field we retain
class El(final val grid:OccupancyGrid, final val name: String, final val color: String, final override val weight: Int) extends Piece {

  private val a = Piece.getBoxTemplateOfSize(grid.rows)

  // blank out the hole in the el

  for {
    i <- 0 to (grid.rows - 2)
    j <- 1 until grid.cols
  } {
    a(i)(j) = false
    grid.unoccupy(i,j)
  }

  val colorGrid: Array[Array[Cell]] = Piece.getColorGrid(color, a)
}

object Piece {

  def getLinearGrid(length:Int): OccupancyGrid = getGrid(1, length, filled = true)
  def getBoxGrid(size:Int): OccupancyGrid = getGrid(size,size, filled = true)
  def getGrid(rows:Int, cols:Int, filled:Boolean):OccupancyGrid = OccupancyGrid(rows, cols, filled)

  // this is called by board piece constructors to get their layout
  def getColorGrid(color: String, template: Array[Array[Boolean]]): Array[Array[Cell]] = {
    getLayoutImpl(color, template)
  }

  def getBoardColorGrid(color: String, size: Int): Array[Array[Cell]] = {
    require(size >= 5, "board's gotta be larger than biggest piece (1x5 or 5x1)")

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

    class Rotated(
      val name:            String,
      val color:           String,
      val colorGrid:       Array[Array[Cell]],
      val grid:            OccupancyGrid,
      override val weight: Int
    ) extends Piece

    val rotatedColorGrid = pieceToCopy.colorGrid.transpose.map(_.reverse).map(_.toArray)

    // worst transformation ever - let's hope it's worth it
    val rotatedGrid = pieceToCopy.grid.rotate

    new Rotated(newName, pieceToCopy.color, rotatedColorGrid, rotatedGrid, pieceToCopy.weight)

  }

  def getBoxTemplateOfSize(size: Int): Array[Array[Boolean]] = Array.tabulate(size, size) { (_, _) => true }

}