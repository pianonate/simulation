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
class Board(
  final val name:          String,
  final val color:         String,
  final val grid:          OccupancyGrid,
  final val colorGrid:     Array[Array[String]],
  final val neighborsArray: Array[Array[Int]],
  final val specification: Specification
) extends Piece {

  def this(size: Int, specification: Specification) {
    // initial board creation just requires a size - initialize with all proper defaults
    this(
      "Board",
      Board.BOARD_COLOR,
      OccupancyGrid(size, size, filled = false),
      Board.getBoardColorGrid,
      Board.copyNeighborsArray(Board.initNeighborsArray),
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

  def islandMax: Int = findLargestConnectedComponent

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
      grid.clearCol(col)

      // clear all col positions in each row
      var row = 0
      while (row < rows) {
        colorGrid(row)(col) = "" // Cell(this.color)
        updateNeighbors(Loc(row, col))
        row += 1
      }
      clearedCols += 1
    }

    def clearRow(row: Int, rowCells: Array[String]): Unit = {

      grid.clearRow(row)

      var col = 0
      while (col < rowCells.length) {
        rowCells(col) = "" // Cell(this.color)
        updateNeighbors(Loc(row, col))
        col += 1
      }
      clearedRows += 1
    }

    val clearableRows = grid.fullRows
    val clearableCols = grid.fullCols

    val fClearRow = (i: Int) => clearRow(i, this.colorGrid(i))
    val fClearCol = (i: Int) => clearCol(i)

    def clear(s: Array[Long], f: Int => Unit): Unit = {
      var i = 0

      // -1 = total hack to avoid cost of using splitAt in fullRows and fullCols
      // this will be encapsulated in OccupancyGrid once we
      while (i < s.length && s(i) > -1) {
        val n = s(i).toInt
        f(n)
        i += 1
      }
    }

    clear(clearableRows, fClearRow)
    clear(clearableCols, fClearCol)
    /*
    var i = 0
    while (i < clearableRows.length && clearableRows(i) > -1) {
      val rowToClear = clearableRows(i)
      grid.clearRow(rowToClear)
      clearedRows += 1
      i += 1
    }*/

    // rows cleared and cols cleared
    //(clearableRows.length, clearableCols.length)
    (clearedRows, clearedCols)
  }

  // on Mac Pro
  // at the end of every simulation
  // count neighbors by going through each location and counting
  // countNeighbors:  42%,      3,997/s
  // place         :    2%,    74,050/s
  // simulations   :   62%,     2,907/s (using pieceSequenceSimulation$1adapted
  //
  // switching to maintaining an array of neighbor counts
  // and now countNeighbors simply gets the current values from the array
  // countNeighbors:    0%, 1,627,580/s
  // place         :   13%,    22,195/s
  // simulations   :   42%,     6,955/s
  private[this] def updateNeighbors(loc: Loc): Unit = {

    // todo - if this works, turn allLocationNeighbors into a map indexed on Loc
    val neighbors = Board.allLocationNeighbors(loc.row * 10 + loc.col)

    // zero out the current position as it has just been placed with a piece
    // or if it's just cleared then it needs to count from 0
    this.neighborsArray(loc.row)(loc.col) = 0

    def updateNeighbor(tryLoc: Loc, neighbors: Array[Loc]) = {
      var i = 0
      while (i < neighbors.length) {
        val neighbor = neighbors(i)
        if (isNeighbor(neighbor)) {
          this.neighborsArray(tryLoc.row)(tryLoc.col) += 1
        }
        i += 1
      }
    }

    var i = 0

    while (i < neighbors.length) {
      val tryLoc = neighbors(i)

      // try each neighbor - first ignore out of bounds
      // provide isOutOfBounds information at construction of allLocationNeighbors - why recalculate each time?
      if (!Board.isOutOfBounds(tryLoc)) {
        // if the neighbor is inbounds, and it's occupied it's count was already set to 0
        if (cachedOccupancyGrid(tryLoc.row)(tryLoc.col))
          ()
        else {
          // reset this value so you can count up its current neighbors
          this.neighborsArray(tryLoc.row)(tryLoc.col) = 0
          updateNeighbor(tryLoc, Board.allLocationNeighbors(tryLoc.row * 10 + tryLoc.col))
        }
      }
      i += 1
    }

    // if it's just been set then update the current position to 0
    // otherwise count it as well
    // todo - move neighborsArray into OccupancyGrid?
    if (!cachedOccupancyGrid(loc.row)(loc.col))
      // todo - allLocationNeighbors can be constructed as a map?
      updateNeighbor(loc, neighbors)
  }

  // start optimization
  // Execution Time: 26%, 5,212/s
  //
  // eliminate for comprehension version / replace with tail recur
  // Execution Time: 2%, 108,793/s - 1900% speedup
  def place(piece: Piece, loc: Loc): Unit = {

    val locRow = loc.row
    val locCol = loc.col

    val pieceRows = piece.rows
    val pieceCols = piece.cols
    val pieceGrid = piece.cachedOccupancyGrid

    def checkCell(row: Int, col: Int): Unit = {
      val color = piece.color
      if (pieceGrid(row)(col)) {
        val replaceColor = color // Cell(color)
        val (i, j) = (row + locRow, col + locCol)
        this.colorGrid(i)(j) = replaceColor
        this.grid.occupy(i, j)
        updateNeighbors(Loc(i, j))
      }
    }

    @annotation.tailrec def placeLoop(row: Int, col: Int): Unit = {
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
      if (legalPlacement(piece, locs(i))) {
        buf(n) = locs(i)
        n += 1
      }
      i += 1
    }

    // now 30% of the time is spent just doing a splitAt - maybe we can remove...
    // removed buf.splitAt by just copying buf to a new resultBuf array
    // went from 14,955/s to 21,692/s on macbook air
    // 45% faster!
    val resultBuf = new Array[Loc](n)
    i = 0
    while (i < n) {
      resultBuf(i) = buf(i)
      i += 1
    }

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
        val pieceOccupied = pieceGrid(r)(c)
        val boardOccupied = cachedOccupancyGrid(r + locRow)(c + locCol)
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
  private[this] def findLargestConnectedComponent: Int = {

    val labels = Board.boardSizeIntArray
    val labelsLength = labels.length

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
    def isSafe(loc: Loc): Boolean = {

      if (Board.isOutOfBounds(loc))
        return false

      // has it been visited?
      if (labels(loc.row)(loc.col) > 0)
        return false

      // is it occupied?
      if (cachedOccupancyGrid(loc.row)(loc.col))
        return false

      true

    }

    def labelLocAndNeighbors(loc: Loc, currentLabel: Int): Unit = {

      val row = loc.row
      val col = loc.col
      labels(row)(col) = currentLabel

      val neighbors = Board.allLocationNeighbors((row * 10) + col)

      var i = 0
      val length = neighbors.length
      while (i < length) {
        val tryLoc: Loc = neighbors(i)
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

  /**
   * this bad boy takes 38% of the execution time when islandMax is not in the picture
   * so...stop counting it after, keep it updated during
   * the theory is - if you maintain the neighborArray at placement/clearLines time rather
   * than counting neighbors after, then you'll be much better off
   * start of optimization
   * Execution: 38%, MacBookAir: 279K / second
   *
   * DONE - see updateNeighbors
   *
   * Note:  You have to short circuit on cachedOccupancyGrid otherwise you'll get an ArrayIndexOutOfBounds exception checking cachedOccupancyGrid
   *        on an out of bounds position
   *
   * Note also: isOutOFBounds only takes about 1/3 of the time of this call - I don't know why it takes about 5% of program execution time
   *            to lookup a loc.row/loc.col in the cachedOccupancyGrid
   */
  private[this] def isNeighbor(loc: Loc): Boolean = Board.isOutOfBounds(loc) || cachedOccupancyGrid(loc.row)(loc.col)

  private[this] def countNeighbors(locs: Array[Loc]): Array[Int] = {

    val counts = Array(0, 0, 0, 0, 0)
    val locLength = locs.length

    var i = 0
    while (i < locLength) {
      val count = this.neighborsArray(locs(i).row)(locs(i).col)
      counts(count) += 1
      i += 1
    }

    counts

  }

  def results: Array[Int] = {

    def getResultArray = specification.length match {
      case 1 => Array(0)
      case 2 => Array(0, 0)
      case 3 => Array(0, 0, 0)
      case 4 => Array(0, 0, 0, 0)
      case 5 => Array(0, 0, 0, 0, 0)
      case 6 => Array(0, 0, 0, 0, 0, 0)
      case 7 => Array(0, 0, 0, 0, 0, 0, 0)
      case 8 => Array(0, 0, 0, 0, 0, 0, 0, 0)
    }

    // todo: these may not be necessary to call if the specification doesn't require them
    //       turn specification into a class and have it identify whether or not these need to be called
    val neighbors = this.neighborCount

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
  private val BOARD_COLOR = Game.BRIGHT_WHITE

  val BOX_CHAR:String = "\u25A0" + Game.SANE
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

  private val allLocationsList: List[Loc] = getLocations(BOARD_SIZE)
  private val allLocations: Array[Loc] = allLocationsList.toArray
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

  private def colorGridTemplate: Array[Array[String]] = new Array[Array[String]](BOARD_SIZE)

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

    val newNeighborsArray = copyNeighborsArray(boardToCopy.neighborsArray)

    new Board(newName,  BOARD_COLOR, boardToCopy.grid.copy, newColorGrid, newNeighborsArray, boardToCopy.specification)

  }

  // prior to this would clone the following array and return - but this one method
  // was taking up 15% of overall execution time.
  // even when constructing an array with fill, took a high % (i didn't record)
  private def boardSizeIntArray: Array[Array[Int]] = // Array.fill(10)(Array.fill[Int](10)(0))
    {
      val a = new Array[Array[Int]](10)
      var i = 0
      while (i < BOARD_SIZE) {
        a(i) = new Array[Int](10)
        i += 1
      }
      a
    }

  /**
   * isOutOfBounds is agnostic of any particular board,
   * so it's a helper function on the object
   */
  private def isOutOfBounds(loc: Loc): Boolean = {

    val row = loc.row
    val col = loc.col

    row == BOARD_SIZE || col == BOARD_SIZE || row == -1 || col == -1
  }

  /**
   * an empty board has an initial count of neighbors
   * every position on the wall has 1 neighbor (the wall)
   * and every corner hsa two wall neighbors
   */
  private val initNeighborsArray: Array[Array[Int]] = {

    def initCountNeighbors(loc: Loc): Int = {
      directions
        .map(dir => isOutOfBounds(Loc(loc.row + dir.row, loc.col + dir.col)))
        .count(_ == true)
    }

    val a = boardSizeIntArray

    for {
      i <- a.indices
      j <- a(0).indices
      loc = Loc(i, j)
    } {
      a(i)(j) = initCountNeighbors(loc)
    }
    a

  }

  /**
   * clone the initial neighbor array so every  board gets their
   * own copy upon construction
   */
  private def copyNeighborsArray(source:Array[Array[Int]]): Array[Array[Int]] = {

    val a = new Array[Array[Int]](BOARD_SIZE)
    var i = 0
    while (i < source.length) {
      a(i) = source(i).clone
      i += 1
    }

    a
  }

  private def getBoardColorGrid: Array[Array[String]] = {
    Array.tabulate(BOARD_SIZE, BOARD_SIZE) { (_, _) => "" /*new Cell(BOARD_COLOR)*/ }
  }

}
