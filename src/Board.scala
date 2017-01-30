/**
 * Created by nathan on 12/9/16.
 *
 * Board is the game Board plus helper functions.
 * It's like other pieces in that it has a name, and a layout and a color (which is the color when first instantiated)
 *
 * Boards can be created at will in order to test simulations
 * There will always be the main board held by the Game but other Boards will be created when running simulations.
 * rather a lot of them, I'd say.
 *
 */
// primary constructor mostly called on copy. alternate is called once - for the board used in the game

case class ClearedLines(rows: Int, cols: Int)

class Board(
  final val name:      String,
  final val color:     String,
  final val grid:      OccupancyGrid,
  final val colorGrid: Array[Array[String]],
  //final val neighborsArray: Array[Array[Int]],
  final val specification: Specification
) extends Piece {

  def this(size: Int, specification: Specification) {
    // initial board creation just requires a size - initialize with all proper defaults
    this(
      "Board",
      Board.BOARD_COLOR,
      OccupancyGrid(size, size, filled = false),
      Board.getBoardColorGrid,
      specification
    )
  }

  // the board output shows unoccupied cells so just call .show on every cell
  // different than the Piece.show which will not output unoccupied Cell's in other pieces
  // this method is mapped in from Piece.show
  override def cellShowFunction(row: Int, col: Int): String = {
    val occupied = cachedOccupancyGrid(row)(col)
    val color = colorGrid(row)(col)

    if (occupied)
      color + Board.BOX_CHAR
    else
      Board.UNOCCUPIED_BOX_CHAR
  }

  def maximizerCount: Int = legalPlacements(Specification.maximizer).length


  // changed to not use a rotated copy of the board
  // slight increase in LOC but definite decrease in % of code execution time
  // from ~24% with the original version - now it's 226102 / 1543684 = 0.146469096 = 14.6% of code execution time
  //
  // next level of optimization.
  // start: clearLines takes 58% of placeMe at 2,992 per second in the profiler
  // fullRow and fullCol eliminate forall:  now 35% of placeMe at 5,344 per second in the profiler
  // fullRows and fullCols built with tail recursion: now 4% of placeMe at 89,635 / second in the profiler
  // final  removed foreach on clearableRows and clearableCols:  3% of placeMe at 104,000 / second in the profiler
  // overall improvement = 3300%
  //
  // at start of this optimization, clearLines was 11% of overall execution time at end, it was .3%
  // this is a super strong argument for using while loops and buildings things tail recursively when performance
  // is on the line in tight loops
  //
  // much further down the road - sped this up by 180% by removing lambdas,
  // optimizing the sentinel creation in calls to grid.fullRows, grid.fulLCols
  def clearLines(clearColor:Boolean): ClearedLines = {

    //hmmm - so clearColor is only necessary on the real board, but not on simulations.  We don't use
    //       colorgrid to determine occupancy anymore so...

    // todo - can delegate to OccupancyGrid once we get rid of colorGrid.occupied
    // maybe not because the color grid is updated as part of this.
    // think about it a spell

    def clearCol(col: Int): Unit = { /*{ for (i <- layout.indices) layout(i)(col) = new Cell(false, this.color, true) }*/
      grid.clearCol(col)

      if (clearColor) {
        // clear all col positions in each row
        var row = 0
        while (row < rows) {
          colorGrid(row)(col) = ""
          row += 1
        }
      }
    }

    def clearRow(row: Int, rowColorCells: Array[String]): Unit = {

      grid.clearRow(row)

      if (clearColor) {
        var col = 0
        while (col < cols) {
          rowColorCells(col) = ""
          col += 1
        }
      }
    }

    val clearableRows = grid.fullRows
    val clearableCols = grid.fullCols

    // 57% speed up of this method by getting rid of the lambdas
    // used to eliminate separate clearRows/clearCols
    // shee-ite

    def clearRows(rowsToClear: Array[Long]): Int = {
      var i = 0
      while (i < rowsToClear.length && rowsToClear(i) > -1) {
        val n = rowsToClear(i).toInt
        clearRow(n, this.colorGrid(i))
        i += 1
      }
      i
    }

    def clearCols(colsToClear: Array[Long]): Int = {
      var i = 0
      while (i < colsToClear.length && colsToClear(i) > -1) {
        val n = colsToClear(i).toInt
        clearCol(n)
        i += 1
      }
      i
    }

    val clearedRows = clearRows(clearableRows)
    val clearedCols = clearCols(clearableCols)

    // rows cleared and cols cleared
    //(clearableRows.length, clearableCols.length)
    ClearedLines(clearedRows, clearedCols)
  }

  // start optimization
  // Execution Time: 26%, 5,212/s
  //
  // eliminate for comprehension version / replace with tail recur
  // Execution Time: 2%, 108,793/s - 1900% speedup
  def place(piece: Piece, loc: Loc, updateColor:Boolean): Unit = {

    val locRow = loc.row
    val locCol = loc.col

    val pieceRows = piece.rows
    val pieceCols = piece.cols
    val pieceGrid = piece.cachedOccupancyGrid
    val replaceColor = piece.color


    def checkCell(row: Int, col: Int): Unit = {
      if (pieceGrid(row)(col)) {

        // it turns out that initializing like this
        // val (i, j) = (row + locRow, col + locCol)
        // took 3% of execution time.  christ.
        val i = row + locRow
        val j = col + locCol

        if (updateColor) this.colorGrid(i)(j) = replaceColor
        this.grid.occupy(i, j)

      }
    }

    var r = 0
    var c = 0
    while (r < pieceRows) {
      while (c < pieceCols) {
        checkCell(r, c)
        c += 1
      }

      c = 0
      r += 1
    }

  }

  // optimize this
  // at start (% is of total execution time)
  // Execution Time: 15%, 2,125/s
  //
  // first remove the toList which was required by using allLocations
  // added a lazyVal on board to get an allLocations list (which is only computed on first use)
  // we'll see what this thing looks like with toList gone
  //
  // Execution Time: 14%, 2,281/s
  //
  // now add tail recursion creation of legalPlacement locations to get rid of the
  // current for comprehension used to build the list of legals
  //
  // Execution Time: 1%, 59,904/s - a 2500% increase of this section
  //
  // now that so many other things have been optimized, legalPlacements
  // takes ~12% (Own Time) out of all execution time
  // i don't see how this could be optimized any further

  def legalPlacements(piece: Piece): Array[Loc] = {
    // walk through each position on the board
    // see if the piece fits at that position, if it does, add that position to the list
    /*for { loc <- Board.allLocationsList.par if legalPlacement(piece, loc) } yield loc */

    // moving to array and cleaning up lazy vals in Piece
    // made this thing scream - 10x faster
    var i = 0
    var n = 0
    val locs = Board.allLocations
    val buf = new Array[Loc](locs.length)

    while (i < locs.length) {
      val loc = locs(i)
      if (legalPlacement(piece, loc)) {
        buf(n) = loc
        n += 1
      }
      i += 1
    }

    // now 30% of the time is spent just doing a splitAt - maybe we can remove...
    // removed buf.splitAt by just copying buf to a new resultBuf array
    // went from 14,955/s to 21,692/s on macbook air
    // 45% faster!
    val resultBuf = new Array[Loc](n)
    Array.copy(buf, 0, resultBuf, 0, n)

    resultBuf
  }

  private[this] def legalPlacement(piece: Piece, loc: Loc): Boolean = {

    // 607K/s with the returns inline vs. 509K/s with the returns removed.  the returns stay
    val locRow = loc.row
    val locCol = loc.col
    val pieceRows = piece.rows
    val pieceCols = piece.cols

    if ((pieceRows + locRow) > rows) return false // exceeds the bounds of the board - no point in checking any further
    if ((pieceCols + locCol) > cols) return false // exceeds the bounds of the board - no point in checking any further

    val pieceGrid = piece.cachedOccupancyGrid

    // find all instances
    // where the piece has an occupied value and the board has an occupied value - that is illegal, so bail
    // otherwise it's leg

    // using a while loop rather than a for comprehension because
    // the while loop is a LOT faster
    var r = 0
    var c = 0
    while (r < pieceRows) {
      while (c < pieceCols) {
        // okay - i don't know why in-lining isn't happening by the compiler
        // but checking this in the profiler - in-lining makes this go a lot faster
        // also, the cachedOccupancyGrid is usually mostly empty so this should short-circuit
        // profiler does say it goes faster
        if ( /*cachedOccupancyGrid*/ grid.occupancyGrid(r + locRow)(c + locCol) && pieceGrid(r)(c)) {
          return false
        }
        c += 1
      }
      c = 0
      r += 1
    }

    // if we didn't bail, then this piece placement is legal
    true

  }

  private def countNeighbors(locs: Array[Loc]): Array[Int] = {

    val counts = Array(0, 0, 0, 0, 0)
    val locLength = locs.length

        // walk through all directions
    def countLocationNeighbor(loc: Loc, locNeighbors: Array[Loc]): Unit = {

      val length = locNeighbors.length
      var i = 0
      var count = 0
      while (i < length) {
        val tryLoc = locNeighbors(i)

        val row = tryLoc.row
        val col = tryLoc.col

        // if it's out of bounds or the neighbor is on...
        // moving this boolean test inline sped up countLocationNeighbor dramatically by, 20x
        // wtf
        // given other tests with YourKit, I'm not sure that it's reporting this correctly.  it may be that it's a heisenberg issue
        if (row == Board.BOARD_SIZE || col == Board.BOARD_SIZE || row == -1 || col == -1 || cachedOccupancyGrid(row)(col)) { count += 1 }

        i += 1
      }

      counts(count) += 1

    }

    def countAllLocationNeighbors() = {
      var i = 0
      while (i < locLength) {
        val loc = locs(i)
        if (cachedOccupancyGrid(loc.row)(loc.col))
          ()
        else {

          countLocationNeighbor(loc, Board.allLocationNeighbors(i))

        }

        i += 1
      }
    }

    countAllLocationNeighbors()

    counts

  }

  private def getResultArray = specification.length match {
    case 5 => Array(0, 0, 0, 0, 0)
    case 6 => Array(0, 0, 0, 0, 0, 0)
    case 7 => Array(0, 0, 0, 0, 0, 0, 0)
  }

  def emptyResults:Array[Int] = getResultArray

  def results: Array[Int] = {

    val neighbors = this.countNeighbors(Board.allLocations)

    /*val oldNeighbors =
    assert(neighbors(1)==oldNeighbors(1) && neighbors(2)==oldNeighbors(2) && neighbors(3)==oldNeighbors(3) && neighbors(4)==oldNeighbors(4))*/

    import Specification._
    def getNamedResult(name: String): Int = {
      name match {
        // multiply times -1 to make compare work without having to
        // move this._ and that._ values around
        case s if s == occupiedCountName  => grid.popCount
        case s if s == maximizerCountName => this.maximizerCount * -1
        case s if s == fourNeighborsName  => neighbors(4)
        case s if s == threeNeighborsName => neighbors(3)
        case s if s == twoNeighborsName   => neighbors(2)
        case s if s == maxContiguousName  => this.grid.maxContiguousOpenLines * -1
        case s if s == openLinesName      => this.grid.openLineCount * -1
      }
    }

    // getResults is 3,915/second with while loop
    // vs. 3,133/second with this map - good golly
    //specification.map(spec => getResult(spec.fieldName))

    val a = getResultArray
    var i = 0

    //val parspec = specification.spec.par
    // al parspec = specification.spec.zipWithIndex.par
    // parspec.foreach(spec => a(spec._2) = getNamedResult(spec._1.fieldName))

    while (i < specification.length /*parspec.length*/ ) {
      a(i) = getNamedResult(specification(i).fieldName) /*parspec(i).fieldName*/
      i += 1
    }
    a

  }

}

object Board {

  val BOARD_SIZE = 10
  val BOARD_COLOR = StringFormats.BRIGHT_WHITE

  val BOX_CHAR: String = "\u25A0" + StringFormats.SANE

  private val UNOCCUPIED_BOX_CHAR = BOARD_COLOR + BOX_CHAR

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
  private def getLocations(boardSize: Int): List[Loc] = /*Array.tabulate(layout.length, layout.length)((i, j) => (i, j)).flatten.toList*/ {

    @annotation.tailrec def loop(row: Int, col: Int, acc: List[Loc]): List[Loc] = {
      (row, col) match {
        case (-1, _) => acc
        case (_, 0)  => loop(row - 1, boardSize - 1, Loc(row, col) :: acc)
        case _       => loop(row, col - 1, Loc(row, col) :: acc)
      }
    }

    val size = boardSize
    loop(size - 1, size - 1, List())

  }

  private val allLocations: Array[Loc] = getLocations(BOARD_SIZE).toArray
  private val directions = Array(Loc(-1, 0), Loc(0, -1), Loc(1, 0), Loc(0, 1))

  private val allLocationNeighbors: Array[Array[Loc]] = {

    // stashing all location neighbors once at the beginning rather than calculating it each time has saved about 15% of execution time:
    // on countNeighbors alone it sped up the number of times per second by 1948% (~20x)
    def getNeighbors(loc: Loc): Array[Loc] = {
      List.fill[Int](directions.length)(0).indices.map(n => Loc(loc.row + directions(n).row, loc.col + directions(n).col)).toArray
    }

    val a = allLocations.map(getNeighbors)
    a
  }

  //private def colorGridTemplate: Array[Array[String]] = new Array[Array[String]](BOARD_SIZE)

  // todo - copy is only ever called by simulations so don't worry about copying the colorGrid - just keep the same one
  def copy(newName: String, boardToCopy: Board): Board = {

    // mapping the .clone gives a new array
    // it holds all of the same references but if you put a new cell in place the original board's layout doesn't change
    // so this is the good - it will work
    /*val layout = boardToCopy.layout.map(_.clone)*/

    // using while loop rather than map as we went from
    // Execution Time: 10%, 19,527/s
    //
    // To
    // Execution Time:  3%, 69.491/s
    //
    // replaced Array.ofDim with a call to a template method
    // to instantiate a new Array...
    // Execution Time; 1.6%, 116,579/s ~500% improvement over start
   /* val newColorGrid = colorGridTemplate

    var i = 0
    while (i < BOARD_SIZE) {
      newColorGrid(i) = boardToCopy.colorGrid(i).clone
      i += 1
    }
*/
    new Board(newName, BOARD_COLOR, boardToCopy.grid.copy, boardToCopy.colorGrid, boardToCopy.specification)

  }

  private def getBoardColorGrid: Array[Array[String]] = {
    Array.tabulate(BOARD_SIZE, BOARD_SIZE) { (_, _) => "" /*new Cell(BOARD_COLOR)*/ }
  }

}
