/**
 * Created by nathan on 12/9/16.
 * Board is the game Board plus helper functions.
 * It's like other pieces in that it has a name, and a layout and a color (which is the color when first instantiated)
 *
 * Boards can be created at will to test out different combinations.
 * THere will always be the main board held by the Game but other Board objects will be created when running algos
 */
import scala.annotation.tailrec

class Board( final val colorGrid: Array[Array[Cell]], final val grid: OccupancyGrid, final val name: String, final val color: String) extends Piece {

  def this(size: Int) {
    // initial board creation just requires a size
    this(Piece.getBoardColorGrid(Board.BOARD_COLOR, size), OccupancyGrid(size, size, filled = false), "Board", Board.BOARD_COLOR)
  }

  def this(newColorGrid: Array[Array[Cell]], newGrid: OccupancyGrid, name: String) {
    // provides ability to name a board copy
    this(newColorGrid, newGrid, name, Board.BOARD_COLOR)
  }

  // the board output shows unoccupied cells so just call .show on every cell
  // different than the Piece.show which will not output unoccupied Cell's in other pieces
  // this method is mapped in from Piece.show
  override def cellShowMapFunction(cell: Cell): String = cell.showForBoard

  def maximizerCount: Int = legalPlacements(Simulation.maximizer).length

  def islandMax: Int = findLargestConnectedComponent

  def occupiedCount: Int = getOccupiedPositions

  // private def getNeighborCount: Array[Int] = countNeighbors(Board.allLocations)
  def neighborCount: Array[Int] = countNeighbors(Board.allLocations)

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
  def clearLines(): (Int, Int) = {

    // todo - can delegate to OccupancyGrid once we get rid of colorGrid.occupied

    var clearedRows = 0
    var clearedCols = 0

    def clearCol(col: Int): Unit = { /*{ for (i <- layout.indices) layout(i)(col) = new Cell(false, this.color, true) }*/
      // clear all col positions in each row
      var row = 0
      while (row < colorGrid.length) {
        colorGrid(row)(col) = Cell(occupied = false, this.color)
        row += 1
      }
      grid.clearCol(col)
      clearedCols += 1
    }

    def clearRow(row: Array[Cell]): Unit = {
      var i = 0
      while (i < row.length) {
        row(i) = Cell(occupied = false, this.color)
        i += 1
      }
    }

    val clearableRows = grid.fullRows
    val clearableCols = grid.fullCols

    val fClearRow = (i: Int) => clearRow(this.colorGrid(i))
    val fClearCol = (i: Int) => clearCol(i)

    def clear(s: Seq[Int], f: Int => Unit): Unit = {
      var i = 0
      val length = s.size
      // -1 = total hack to avoid cost of using splitAt in fullRows and fullCols
      // this will be encapsulated in OccupancyGrid once we
      while (i < length && s(i) > - 1) {
        // todo get rid of colorGrid
        f(s(i))

        i += 1
      }
    }

    clear(clearableRows, fClearRow)
    clear(clearableCols, fClearCol)

    var i = 0
    while (i < clearableRows.length && clearableRows(i) > -1) {
      val rowToClear = clearableRows(i)
      grid.clearRow(rowToClear)
      clearedRows += 1
      i += 1
    }

    // rows cleared and cols cleared
    //(clearableRows.length, clearableCols.length)
    (clearedRows, clearedCols)
  }

  /*// called so rarely that it doesn't need optimization
  def clearPieceUnderlines(piece: Piece, loc: (Int, Int)): Unit = {

    val pieceRowStart = loc._1
    val pieceRowEnd = pieceRowStart + piece.colorGrid.length
    val pieceColStart = loc._2
    val pieceColEnd = pieceColStart + piece.colorGrid(0).length

    for {
      row <- pieceRowStart until pieceRowEnd
      col <- pieceColStart until pieceColEnd
      cell = colorGrid(row)(col)
      if cell.underline
    } cell.underline = false

  }*/

  // start optimization
  // Execution Time: 26%, 5,212/s
  //
  // eliminate for comprehension version / replace with tail recur
  // Execution Time: 2%, 108,793/s - 1900% speedup
  def place(piece: Piece, loc: (Int, Int)): Unit = {

    val (locRow, locCol) = loc

    val pieceRows = piece.rows
    val pieceCols = piece.cols
    val pieceGrid = piece.occupancyGrid

    def checkCell(row: Int, col: Int): Unit = {
      val cell = piece.colorGrid(row)(col)
      if (pieceGrid(row)(col)) {
        val replaceCell = Cell(occupied = true, cell.color)
        val (i, j) = (row + locRow, col + locCol)
        this.colorGrid(i)(j) = replaceCell
        this.grid.occupy(i, j)
      }
    }

    @tailrec def placeLoop(row: Int, col: Int): Unit = {
      (row, col) match {
        case (-1, _) => Unit
        case (_, 0) =>
          checkCell(row, col); placeLoop(row - 1, pieceCols - 1)
        case _ => checkCell(row, col); placeLoop(row, col - 1)
      }
    }

    placeLoop(pieceRows - 1, pieceCols - 1)

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
  def legalPlacements(piece: Piece): Array[(Int, Int)] = {
    // walk through each position on the board
    // see if the piece fits at that position, if it does, add that position to the list
    /*for { loc <- Board.allLocationsList.par if legalPlacement(piece, loc) } yield loc */

    /*    @tailrec def legalPlacements1(locs: List[(Int, Int)], acc: List[(Int, Int)]): List[(Int, Int)] = {
      locs match {
        case head :: tail =>
          if (legalPlacement(piece, head))
            legalPlacements1(tail, head :: acc)
          else
            legalPlacements1(tail, acc)
        case Nil => acc
      }
    }


    // building a list tail recursively so the result is a list that
    // can be used to drive the simulation loop - which turns the list into a ParSeq to
    // take advantage of multiple CPU cores and generate many more simulations / second
    legalPlacements1(Board.allLocationsList, List())*/

    // moving to array and cleaning up lazy vals in Piece
    // made this thing scream - 10x faster
    // now 30% of the time is spent just doing the splitAt - maybe we can
    // todo - just fill the empty values with a sentinel?
    var i = 0
    var n = 0
    val locs = Board.allLocations
    val buf = new Array[(Int, Int)](locs.length)

    while (i < locs.length) {
      if (legalPlacement(piece, locs(i))) {
        buf(n) = locs(i)
        n += 1
      }
      i += 1
    }

    val result = buf.splitAt(n)._1 /*.toList*/
    result
  }

  // todo, eliminate the return
  private def legalPlacement(piece: Piece, loc: (Int, Int)): Boolean = {

    // 607K/s with the returns inline vs. 509K/s with the returns removed.  the returns stay
    val locRow = loc._1
    val locCol = loc._2
    val pieceRows = piece.rows
    val pieceCols = piece.cols

    if ((pieceRows + locRow) > rows) return false // exceeds the bounds of the board - no point in checking any further
    if ((pieceCols + locCol) > cols) return false // exceeds the bounds of the board - no point in checking any further

    val pieceGrid = piece.occupancyGrid

    // find all instances
    // where the piece has an occupied value and the board has an occupied value - that is illegal, so bail
    // otherwise it's leg

    // using a while loop rather than a for comprehension because
    // the while loop is a LOT faster
    var r = 0
    var c = 0
    while (r < pieceRows) {
      while (c < pieceCols) {
        val pieceOccupied = pieceGrid(r)(c)
        val boardOccupied = occupancyGrid(r + locRow)(c + locCol)
        if (pieceOccupied && boardOccupied) {
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

  // prior to this would clone the following array and return - but this one method
  // was taking up 15% of overall execution time.
  // even when constructing an array with fill, took a high % (i didn't record)
  // So now, just return a predefined Array
  // as this takes almost no time at all

  private def connectedCountLabelsArray = // Array.fill(10)(Array.fill[Int](10)(0))
    Array(
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    )

  private val labelsLength = connectedCountLabelsArray.length

  private def isOutOfBounds(loc: (Int, Int)) = {
    val row = loc._1
    val col = loc._2

    row >= occupancyGrid.length || col >= occupancyGrid(0).length || row < 0 || col < 0
  }

  // find largest connected component on the board
  // based on https://en.wikipedia.org/wiki/Connected-component_labeling#One_component_at_a_time
  // except not using a queue, but rather doing a recursive search
  //
  // start optimization
  // ExecutionTime: 27%, 2,542/second
  //
  // Moved getVisitedArray out and made a method that clones the original
  // ExecutionTime: 31%, 3,419/second (apparently execution time went up as a % but perf is better)
  //
  // getVisitedArray now returns an explicitly defined Array - dramatically speeds execution
  // ExecutionTime: 18%, 7,121/second
  //
  // round 2
  // ExecutionTime: 30%, 6,719/s (expected Time(ms) to go up as a % given other optimizations but strangely, per second regressed)
  //
  // now get rid of the ListBuffer appends
  // Execution time: 16%, 12,585/second 395% speed up from unoptimized
  //
  // next thing to do is to use the labelling mechanism of findComponents and also
  // return the max directly by only accumulating the largest island as you go
  // then you will only be updating one labels array and don't have to go through the max
  // calculation
  // regressed to 10,603 after switching to labelling mechanism.  but it uses a list, so maybe use an array?
  //
  private def findLargestConnectedComponent: Int = {

    val labels = connectedCountLabelsArray

    def getComponentCount(label: Int): Int = {

      @annotation.tailrec
      def componentCountLoop(row: Int, col: Int, acc: Int): Int = {

        (row, col) match {
          case (-1, _) => acc
          case (_, 0)  => componentCountLoop(row - 1, labelsLength - 1, acc + (if (labels(row)(col) == label) 1 else 0))
          case _       => componentCountLoop(row, col - 1, acc + (if (labels(row)(col) == label) 1 else 0))
        }
      }

      componentCountLoop(labelsLength - 1, labelsLength - 1, 0)

    }

    // is it safe to recurse into this location
    def isSafe(loc: (Int, Int)): Boolean = {

      if (isOutOfBounds(loc))
        return false

      // has it been visited?
      if (labels(loc._1)(loc._2) > 0)
        return false

      // is it occupied?
      if (occupancyGrid(loc._1)(loc._2))
        return false

      true

    }

    def labelLocAndNeighbors(loc: (Int, Int), currentLabel: Int): Unit = {

      val (row, col) = loc
      labels(row)(col) = currentLabel

      val neighbors = Board.allLocationNeighbors((row * 10) + col)

      var i = 0
      val length = neighbors.length
      while (i < length) {
        val tryLoc: (Int, Int) = neighbors(i)
        if (isSafe(tryLoc)) {
          labelLocAndNeighbors(tryLoc, currentLabel)
        }

        i += 1
      }

    }

    var i = 0
    var max = 0
    var componentCount = 0
    val locs = Board.allLocations
    val length = locs.length
    val minimumSizeComponent = length / 2

    while (i < length && max < minimumSizeComponent) {

      val loc = locs(i)

      if (isSafe(loc)) {
        componentCount += 1
        labelLocAndNeighbors(loc, componentCount)
        val size = getComponentCount(componentCount)
        if (size > max) { max = size }
      }
      i += 1
    }

    max

  }

  private def countNeighbors(locs: Array[(Int, Int)]): Array[Int] = {

    val counts = Array(0, 0, 0, 0, 0)
    val locLength = locs.length

    def hasNeighbor(loc: (Int, Int)): Boolean = isOutOfBounds(loc) || occupancyGrid(loc._1)(loc._2)

    // walk through all directions
    def countLocationNeighbor(loc: (Int, Int), locNeighbors: Array[(Int, Int)]): Unit = {

      val length = locNeighbors.length
      var i = 0
      var count = 0
      while (i < length) {
        val tryLoc = locNeighbors(i)

        if (hasNeighbor(tryLoc)) { count += 1 }

        i += 1
      }

      counts(count) += 1

    }

    def countAllLocationNeighbors() = {
      var i = 0
      while (i < locLength) {
        val loc = locs(i)
        if (occupancyGrid(loc._1)(loc._2))
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

  def results: Array[Int] = {

    import Simulation._

    def getResultArray = specification.length match {
      case 1 => Array(0)
      case 2 => Array(0, 0)
      case 3 => Array(0, 0, 0)
      case 4 => Array(0, 0, 0, 0)
      case 5 => Array(0, 0, 0, 0, 0)
      case 6 => Array(0, 0, 0, 0, 0, 0)
      case 7 => Array(0, 0, 0, 0, 0, 0, 0)
    }

    // todo: these may not be necessary to call if the specification doesn't require them
    //       turn specification into a class and have it identify whether or not these need to be called
    val neighbors = this.neighborCount

    def getNamedResult(name: String): Int = {
      name match {
        // multiply times -1 to make compare work without having to 
        // move this._ and that._ values around
        case s if s == occupiedCountName  => this.occupiedCount
        case s if s == maximizerCountName => this.maximizerCount * -1
        case s if s == fourNeighborsName  => neighbors(4)
        case s if s == threeNeighborsName => neighbors(3)
        case s if s == islandMaxName      => this.islandMax * -1
        case s if s == maxContiguousName  => this.grid.maxContiguousOpenLines * -1
        case s if s == openLinesName      => this.grid.openLineCount * -1
      }
    }

    // getResults is 3,915/second with while loop
    // vs. 3,133/second with this map - good golly
    //specification.map(spec => getResult(spec.fieldName))

    val a = getResultArray
    var i = 0
    while (i < specification.length) {
      a(i) = getNamedResult(specification(i).fieldName)
      i += 1
    }

    a

  }

}

object Board {

  val BOARD_SIZE = 10
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
  private def getLocations(boardSize: Int): List[(Int, Int)] = /*Array.tabulate(layout.length, layout.length)((i, j) => (i, j)).flatten.toList*/ {

    @tailrec def loop(row: Int, col: Int, acc: List[(Int, Int)]): List[(Int, Int)] = {
      (row, col) match {
        case (-1, _) => acc
        case (_, 0)  => loop(row - 1, boardSize - 1, (row, col) :: acc)
        case _       => loop(row, col - 1, (row, col) :: acc)
      }
    }

    val size = boardSize
    loop(size - 1, size - 1, List())

  }

  private val directions = Array((-1, 0), (0, -1), (1, 0), (0, 1))

  private val allLocationsList: List[(Int, Int)] = getLocations(BOARD_SIZE)
  private val allLocations: Array[(Int, Int)] = allLocationsList.toArray

  private val allLocationNeighbors: Array[Array[(Int, Int)]] = {

    // stashing all location neighbors once at the beginning rather than calculating it each time has saved about 15% of execution time:
    // on countNeighbors alone it sped up the number of times per second by 1948% (~20x)
    def getNeighbors(loc: (Int, Int)): Array[(Int, Int)] = {
      List.fill[Int](directions.length)(0).indices.map(n => (loc._1 + directions(n)._1, loc._2 + directions(n)._2)).toArray
    }

    val a = allLocations.map(getNeighbors)
    a
  }

  private val BOARD_COLOR = Game.BRIGHT_WHITE
  // todo - create this without the null, null, null,...
  private def colorGridTemplate: Array[Array[Cell]] = new Array[Array[Cell]](BOARD_SIZE) // Array(null, null, null, null, null, null, null, null, null, null)

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
    val newColorGrid = colorGridTemplate // Array.ofDim[Array[Cell]](Game.BOARD_SIZE)
    var i = 0
    while (i < BOARD_SIZE) {
      newColorGrid(i) = boardToCopy.colorGrid(i).clone
      i += 1
    }

    /* new Board(layout, newName)*/
    new Board(newColorGrid, boardToCopy.grid.copy, newName)

  }
}