/**
 * Created by nathan on 12/9/16.
 * represents pieces in the game - also represents the board
 *
 *
 */
abstract class Piece {
  val name: String
  val color: String
  val grid: OccupancyGrid
  val weight: Int = 0 // defined so subclasses (such as Board) don't have to provide an implementation

  final val usage = Counter()

  final val rows: Int = grid.rows
  final val cols: Int = grid.cols

  // test to see if eliminating the stack frame makes this go faster
  // it does - but i really hate breaking encapsulation - worth it for a
  // 3x improvement in 'isOccupied" - which even still uses about 6% of the code...
  final val cachedOccupancyGrid: Array[Array[Boolean]] = grid.getOccupancyGrid

  // todo - pass in fill string as the board doesn't need it
  // for now:  hack!

  final val printFillString: String = if (cols != Board.BOARD_SIZE) List.fill(cols * 2 + 1)(" ").mkString else "" // used by the game to output a blank row when printed side by side with other pieces

  /**
   * point value is calculated once and used to know the point of a piece
   * it's not just a convenience as we will call pointValue a lot and don't need to
   * recalculate it each time
   */
  final lazy val pointValue: Int = grid.popCount

  override def toString: String = this.name // for the debugger

  def show(f: (Int, Int) => String): String = {

    val s = new StringBuilder()

    for {
      row <- grid.occupancyGrid.indices
      col <- grid.occupancyGrid(row).indices
    } {
      val box = f(row, col)
      val nl = if (col == grid.occupancyGrid(0).length - 1) " \n" else ""
      s ++= box + " " + nl
    }

    // we don't need the final newline
    s.toString.dropRight(1)
  }

  def cellShowFunction(row: Int, col: Int): String = {
    val occupied = grid.occupancyGrid(row)(col)
    if (occupied) color + Board.BOX_CHAR + StringFormats.SANE else " "
  }

}

// generic line class - let's you create a bunch of different lines
// length is not marked as val because it is not a field we retain
case class Line( final val grid: OccupancyGrid, final val name: String, final val color: String, final override val weight: Int) extends Piece

// generic box class - let's you create boxes of different sizes
// size is not marked as val because it is not a field we retain
// although size does matter
case class Box( final val grid: OccupancyGrid, final val name: String, final val color: String, final override val weight: Int) extends Piece

// generic El class - let's you create L's of different sizes
// size is not marked as val because it is not a field we retain
case class El( final val grid: OccupancyGrid, final val name: String, final val color: String, final override val weight: Int) extends Piece {

  // blank out the hole in the el

  for {
    i <- 0 to (grid.rows - 2)
    j <- 1 until grid.cols
  } {
    grid.unoccupy(i, j)
  }

}

object Piece {

  def getLinearGrid(length: Int): OccupancyGrid = getGrid(1, length, filled = true)
  def getBoxGrid(size: Int): OccupancyGrid = getGrid(size, size, filled = true)
  def getGrid(rows: Int, cols: Int, filled: Boolean): OccupancyGrid = OccupancyGrid(rows, cols, filled)

  // take a piece, and create a new piece, rotated 90 degrees
  def rotate90(newName: String, pieceToCopy: Piece): Piece = {

    case class Rotated(
      name:                String,
      color:               String,
      grid:                OccupancyGrid,
      override val weight: Int
    ) extends Piece

    // worst transformation ever - let's hope it's worth it
    val rotatedGrid = pieceToCopy.grid.rotate

    Rotated(newName, pieceToCopy.color, rotatedGrid, pieceToCopy.weight)
  }

}