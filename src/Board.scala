/**
 * Created by nathan on 12/9/16.
 * Board is the game Board plus helper functions.
 * It's like other pieces in that it has a name, and a layout and a color (which is the color when first instantiated)
 *
 * Boards can be created at will to test out different combinations.
 * THere will always be the main board held by the Game but other Board objects will be created when running algos
 */
class Board(val layout: Array[Array[Cell]], val name: String, val color: String) extends Piece {

  // initial board creation just requires a size
  def this(size: Int) {
    this(Piece.getBoardLayout(Board.BOARD_COLOR, size), "Board", Board.BOARD_COLOR)
  }

  // provides ability to name a board copy
  def this(layout: Array[Array[Cell]], name: String) {
    this(layout, name, Board.BOARD_COLOR)
  }

  // the board outputs unoccupied cells so just call toString on every piece
  // different than the Piece.toString which will not output unoccupied Cell's in other pieces
  // this method is mapped in from Piece.toString
  override def cellToStringMapFunction(cell: Cell): String = cell.toString

  // calculate all locations for a board once - at board creation
  private val allLocations: List[(Int, Int)] = GameUtil.getLocationsList[Cell](layout)

  // todo - right now we're using a scale value of 3 - maybe we want to try multiple values such as 3 and 4 and return the sum...
  def entropy: Double = Entropy.scaledEntropy(3, this.layout)

  def islands: Map[Int, Int] = Islands.findIslands(this.layout).groupBy(_.length).mapValues(_.length)

  // changed to not use a rotated copy of the board
  // slight increase in LOC but definite decrease in % of code execution time
  // from ~24% with the original version - now it's 226102 / 1543684 = 0.146469096 = 14.6% of code execution time
  // a substantial improvement
  def clearLines(): (Int, Int) = {

    def clearCol(col: Int): Unit = { for (i <- layout.indices) layout(i)(col) = new Cell(false, this.color, true) }

    def clearRow(row: Array[Cell]): Unit = { for (i <- row.indices) row(i) = new Cell(false, this.color, true) }

    def fullRow(row: Array[Cell]): Boolean = row.forall(cell => cell.occupied)
    def fullCol(col: Int): Boolean = layout.forall(row => row(col).occupied)

    def fullRows(): Seq[Int] = {
      for {
        i <- this.layout.indices
        if fullRow(this.layout(i))
      } yield i
    }

    def fullCols(): Seq[Int] = {
      for {
        i <- this.layout.indices
        if fullCol(i)
      } yield i
    }

    val clearableRows = fullRows()
    val clearableCols = fullCols()

    // an empty line is an empty line stash the precalculated version in there
    // it almost seems as if the re-using empty row
    clearableRows.foreach(i => clearRow(this.layout(i)) /*this.layout(i) = emptyRow*/ )
    clearableCols.foreach(i => clearCol(i))

    // rows cleared and cols cleared
    (clearableRows.length, clearableCols.length)
  }

  // when the simulation can't find a possible location
  // then the this will not be able to place a piece
  // in which case return false
  def placeOrFail(piece: Piece, loc: Option[(Int, Int)]): Unit = loc match {

    case Some(l) =>
      place(piece, l)
    case None => throw GameOver

  }

  def simulatePlacement(piece: Piece, loc: (Int, Int)): Unit = {

    // place the piece on a copy, clear the lines
    // and return the occupied count and location that results from that occupied count
    // we'll then sort the result of all occupied counts to see which one has the lowest value
    // and from there - we're golden
    place(piece, loc)
    this.clearLines()
  }

  // todo: this one could go away if we get rid of tryPlacement and all of that - or maybe not
  // instead the Game will be simulating on legal locations so will just use
  // the other definition of place
  def place(piece: Piece, loc: (Int, Int)): Unit = {

    // todo: evaluate using a view here
    for {
      r <- piece.layout.indices
      c <- piece.layout(0).indices
      cell = piece.layout(r)(c)
      if cell.occupied
    } {
      val replaceCell = new Cell(cell.occupied, cell.color, true)
      this.layout(r + loc._1)(c + loc._2) = replaceCell
    }
  }

  /*  private def unoccupiedCells():List[(Int,Int)] = {

    // removed the filter as it didn't work for pieces such as the BigLowerRightEl next to something it could fit around
    // Todo: maybe you can improve the algo but maybe you do have to just evaluate each location.  think about it first - this might be ok
    val a = for {r <- layout.indices;c <-layout(0).indices/*;if !layout(r)(c).occupied*/} yield (r,c)
    a.toList

  }*/

  def legalPlacements(piece: Piece): List[(Int, Int)] = {
    // walk through each position on the board
    // see if the piece fits at that position, if it does, add that position to the list
    for { loc <- allLocations if legalPlacement(piece, loc) } yield loc

  }

  private def legalPlacement(piece: Piece, loc: (Int, Int)): Boolean = {

    val locRow = loc._1
    val locCol = loc._2

    if ((piece.rows + locRow) > rows) return false // exceeds the bounds of the board - no point in checking any further
    if ((piece.cols + locCol) > cols) return false // exceeds the bounds of the board - no point in checking any further

    // this for comprehension will find all instances
    // where the piece has an occupied value and the board has an unoccupied value
    // to yield the unoccupied status of each corresponding position on the board
    for {
      r <- piece.layout.indices
      c <- piece.layout(0).indices
      pieceOccupied = piece.layout(r)(c).occupied
      boardOccupied = layout(r + locRow)(c + locCol).occupied
      if pieceOccupied

    } {
      // bail immediately if you find an instance where the piece is occupied
      // and the board is also occupied
      if (boardOccupied) return false
    }

    // if we didn't bail, then this place is legal
    true

  }

}

object Board {

  private val BOARD_COLOR = GameUtil.BRIGHT_WHITE

  def copy(newName: String, boardToCopy: Board): Board = {

    // mapping the clones gives a new array
    // it holds all of the same references but it you change a cell the original board's layout doesn't change
    // so this is the goods - it will work
    // oh - and by the way - simulations got about 5 times faster
    val layout = boardToCopy.layout.map(_.clone)
    new Board(layout, newName)

  }
}