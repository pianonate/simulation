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

  // the board outputs unoccupied cells so just call toString on every cell
  // different than the Piece.show which will not output unoccupied Cell's in other pieces
  // this method is mapped in from Piece.show
  override def cellShowMapFunction(cell: Cell): String = cell.show

  // todo - right now we're using a scale value of 3 - maybe we want to try multiple values such as 3 and 4 and return the sum...
  // temporary disable
  def entropy: Double = 0.0 // Entropy.scaledEntropy(3, this.layout)

  private def islands: List[Int]  = Islands.findIslands(this.layout, Board.allLocations)

  def islandMax: Int = islands.max

  def openLines: Int = {

    val openRows = layout.count(row => row.forall(!_.occupied))

    def openCol(col: Int): Boolean = layout.forall(row => !row(col).occupied)

    val openCols: Int = {
      for {
        i <- this.layout.indices
        if openCol(i)
      } yield i
    }.length

    openRows + openCols

  }

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

  def clearPieceUnderlines(piece: Piece, loc: (Int, Int)): Unit = {

    val pieceRowStart = loc._1
    val pieceRowEnd = pieceRowStart + piece.layout.length
    val pieceColStart = loc._2
    val pieceColEnd = pieceColStart + piece.layout(0).length

    for {
      row <- pieceRowStart until pieceRowEnd
      col <- pieceColStart until pieceColEnd
      cell = layout(row)(col)
      if cell.underline
    } cell.underline = false

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

  // todo: see if using toList slows this down a lot
  def legalPlacements(piece: Piece): List[(Int, Int)] = {
    // walk through each position on the board
    // see if the piece fits at that position, if it does, add that position to the list
    val l = for { loc <- Board.allLocations if legalPlacement(piece, loc) } yield loc
    l.toList
  }

  private def legalPlacement(piece: Piece, loc: (Int, Int)): Boolean = {

    val locRow = loc._1
    val locCol = loc._2

    if ((piece.rows + locRow) > rows) return false // exceeds the bounds of the board - no point in checking any further
    if ((piece.cols + locCol) > cols) return false // exceeds the bounds of the board - no point in checking any further

    // find all instances
    // where the piece has an occupied value and the board has an unoccupied value
    // to yield the unoccupied status of each corresponding position on the board

    // using a while loop rather than a for comprehension because even with -optimize,
    // the while loop is a LOT faster
    var r = 0
    var c = 0
    while (r < piece.layout.length) {
      while (c < piece.layout(0).length) {
        val pieceOccupied = piece.layout(r)(c).occupied
        val boardOccupied = layout(r + locRow)(c + locCol).occupied
        if (pieceOccupied && boardOccupied) {
          return false
        }
        c += 1
      }
      c = 0
      r += 1
    }

    // if we didn't bail, then this place is legal
    true

  }

}

object Board {

  // calculate all locations for a board once - at class Board construction
  // copyBoard was originally: 21.5% of execution time with the tabulate functionality
  // moved to a while loop with a ListBuffer and that was...31.6% - worse!!
  // so moved to a recursive loop with a List - which was 9.1%
  // and finally by building the list in reverse, recursion gave it back in order...
  // so we didn't have to call reverse and end up with 4.8% of execution time
  // worth the optimization - from 21.5% to 4.8%!

  // duh - this value is invariant throughout the game
  // moved it to Board object, lazy instantiated it on first use
  // and now it is only calculated once in less than 1 ms
  // probably you can put the tabulate mechanism back if you wish
  //
  // now with getLocations calculated once, copyBoard is about 3.1% of the overall time
  private def getLocations(boardSize: Int): Array[(Int, Int)] = /*Array.tabulate(layout.length, layout.length)((i, j) => (i, j)).flatten.toList*/ {

    def loop(row: Int, col: Int, acc: List[(Int, Int)]): List[(Int, Int)] = {
      (row, col) match {
        case (-1, _) => acc
        case (_, 0) => loop(row - 1, boardSize - 1, (row, col) :: acc)
        case _ => loop(row, col - 1, (row, col) :: acc)
      }
    }

    val size = boardSize
    val locs = loop(size - 1, size - 1, List())
    // changed getLocations to Array so that access is constant time rather than linear time for items in the list
    locs.toArray

  }

  lazy val allLocations: Array[(Int, Int)] = getLocations(Game.BOARD_SIZE)

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