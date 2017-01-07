/**
 * Created by nathan on 12/9/16.
 * Board is the game Board plus helper functions.
 * It's like other pieces in that it has a name, and a layout and a color (which is the color when first instantiated)
 *
 * Boards can be created at will to test out different combinations.
 * THere will always be the main board held by the Game but other Board objects will be created when running algos
 */
import scala.annotation.tailrec

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

  def islandMax: Int = findLargestConnectedComponent(this.layout, Board.allLocationsList)

  def neighborCount: Array[Int] = countNeighbors(Board.allLocations)

  // start optimization run
  // Execution Time: 24%, 3,378/s
  //
  // take same approach as in clearlines - remove the for comprehensions and maps that were in place previously
  // based on earlier experience passing lambdas around slows things down so it so right now just factored out
  // a common test used by both openLines and clearLines
  // Execution Time: .8%, 140,276/s - 4,000% increase - openLines is now inconsequential
  def openLines: (Int, Int) = {
    val openRows = testRows(false)
    val openCols = testCols(false)
    val max = if (openRows._2 > openCols._2) openRows._2 else openCols._2

    ((openRows._1.size + openCols._1.size), max)

  }

  /*
  def testRow(row: Array[Cell], testForFull: Boolean): Boolean = { //row.forall(cell => cell.occupied)

    var i = 0

    // if you find any unoccupied then the row can't be full
    var stopTesting = false

    // replaced a return when unoccupied with a conditional that evaluates for it
    // returns are bad in scala
    // https://tpolecat.github.io/2014/05/09/return.html
    while ((i < row.length) && !stopTesting) {
      if (((testForFull && row(i).unoccupied)) || ((!testForFull && row(i).occupied))) {
        stopTesting = true
      }
      i += 1
    }

    if (stopTesting) // if a cell is unoccupied then we can't be full
      false
    else
      true
  }

  val testR = (i:Int, testForFull:Boolean) => testRow(layout(i), testForFull)

  def testCol(col: Int, testForFull: Boolean): Boolean = { //layout.forall(row => row(col).occupied)
  var i = 0

    // if you find any unoccupied then the row can't be full
    var stopTesting = false

    while ((i < layout.length) && !stopTesting) {
      if (((testForFull && layout(i)(col).unoccupied)) || ((!testForFull && layout(i)(col).occupied))) {
        stopTesting = true // true
      }
      i += 1
    }

    if (stopTesting) // if we had to stop testing then the answer is false
      false
    else
      true

  }

  val testC = (i:Int, testForFull:Boolean) => testCol(i, testForFull)

  // passing in tester lambda rather than keeping the logic separate makes this thing 3x slower
  // it would be nice to get the refactoring but not at the expense of the performance hit
  private def testLine(tester: (Int,Boolean) => Boolean, testForFull:Boolean): Seq[Int] = {
    @tailrec def testLineLoop(index: Int, acc: List[Int]): List[Int] = {
      index match {
        case n if n < layout.length =>
          if (tester(n, testForFull))
            testLineLoop(n + 1, n :: acc)
          else
            testLineLoop(n + 1, acc)
        case _ => acc
      }

    }

    testLineLoop(0, List())
  }
*/

  private def testRows(testForFull: Boolean): (Seq[Int], Int) = {

    def testRow(row: Array[Cell], testForFull: Boolean): Boolean = { //row.forall(cell => cell.occupied)

      var i = 0

      // if you find any unoccupied then the row can't be full
      var stopTesting = false

      // replaced a return when unoccupied with a conditional that evaluates for it
      // returns are bad in scala
      // https://tpolecat.github.io/2014/05/09/return.html
      while ((i < row.length) && !stopTesting) {
        if (((testForFull && row(i).unoccupied)) || ((!testForFull && row(i).occupied))) {
          stopTesting = true
        }
        i += 1
      }

      if (stopTesting) // if a cell is unoccupied then we can't be full
        false
      else
        true
    }

    @tailrec def testRowLoop(index: Int, currentMax: Int, max: Int, acc: List[Int]): (List[Int], Int) = {
      index match {
        case n if n < layout.length =>
          if (testRow(layout(n), testForFull)) {
            //current = 1
            // add 1 to currentMax and see if we need to create a new max
            val newMax = if ((currentMax + 1) > max) currentMax + 1 else max

            testRowLoop(n + 1, currentMax + 1, newMax, n :: acc)
          } else {
            // current = 0
            // reset currentMax, pass along max
            testRowLoop(n + 1, 0, max, acc)
          }
        case _ => (acc, max)
      }

    }

    testRowLoop(0, 0, 0, List())
  }

  private def testCols(testForFull: Boolean): (Seq[Int], Int) = {

    def testCol(col: Int, testForFull: Boolean): Boolean = { //layout.forall(row => row(col).occupied)
      var i = 0

      // if you find any unoccupied then the row can't be full
      var stopTesting = false

      while ((i < layout.length) && !stopTesting) {
        if (((testForFull && layout(i)(col).unoccupied)) || ((!testForFull && layout(i)(col).occupied))) {
          stopTesting = true
        }
        i += 1
      }

      // whenever a test is halted prematurely the test didn't pass
      // bummer that scala isn't fast enough such that I had to write code that requires explanation
      // this could have been written in one or two lines of scala code probably but performance was too slow
      if (stopTesting)
        false
      else
        true

    }

    @tailrec def testColLoop(index: Int, currentMax: Int, max: Int, acc: List[Int]): (List[Int], Int) = {
      index match {
        case n if n < layout.length =>
          if (testCol(n, testForFull)) {
            //current = 1
            // add 1 to currentMax and see if we need to create a new max
            val newMax = if ((currentMax + 1) > max) currentMax + 1 else max

            testColLoop(n + 1, currentMax + 1, newMax, n :: acc)
          } else {
            // current = 0
            // reset currentMax, pass along max
            testColLoop(n + 1, 0, max, acc)
          }
        case _ => (acc, max)
      }

    }

    testColLoop(0, 0, 0, List())
  }

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

    def clearCol(col: Int): Unit = { /*{ for (i <- layout.indices) layout(i)(col) = new Cell(false, this.color, true) }*/
      var i = 0
      while (i < layout.length) {
        layout(i)(col) = new Cell(false, this.color, true)
        i += 1
      }
    }

    def clearRow(row: Array[Cell]): Unit = { /*{ for (i <- row.indices) row(i) = new Cell(false, this.color, true) }*/
      var i = 0
      while (i < row.length) {
        row(i) = new Cell(false, this.color, true)
        i += 1
      }
    }

    val clearableRows = testRows(true)._1
    val clearableCols = testCols(true)._1

    val fClearRow = (i: Int) => clearRow(this.layout(i))
    val fClearCol = (i: Int) => clearCol(i)

    def clear(s: Seq[Int], f: Int => Unit): Unit = {
      var i = 0
      val length = s.size
      while (i < length) {
        f(s(i))
        i += 1
      }
    }

    clear(clearableRows, fClearRow)
    clear(clearableCols, fClearCol)

    // rows cleared and cols cleared
    (clearableRows.length, clearableCols.length)
  }

  // called so rarely that it doesn't need optimization
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

  // start optimization
  // Execution Time: 26%, 5,212/s
  //
  // eliminate for comprehension version / replace with tail recur
  // Execution Time: 2%, 108,793/s - 1900% speedup
  def place(piece: Piece, loc: (Int, Int)): Unit = {

    val (locRow, locCol) = loc

    val rows = piece.layout.length
    val cols = piece.layout(0).length

    def checkCell(row: Int, col: Int): Unit = {
      val cell = piece.layout(row)(col)
      if (cell.occupied) {
        val replaceCell = new Cell(cell.occupied, cell.color, true)
        this.layout(row + locRow)(col + locCol) = replaceCell
      }
    }

    @tailrec def placeLoop(row: Int, col: Int): Unit = {
      (row, col) match {
        case (-1, _) => Unit
        case (_, 0) => { checkCell(row, col); placeLoop(row - 1, cols - 1) }
        case _ => checkCell(row, col); placeLoop(row, col - 1)
      }
    }

    placeLoop(rows - 1, cols - 1)

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
  def legalPlacements(piece: Piece): List[(Int, Int)] = {
    // walk through each position on the board
    // see if the piece fits at that position, if it does, add that position to the list
    /*for { loc <- Board.allLocationsList.par if legalPlacement(piece, loc) } yield loc */

    @tailrec def legalPlacements1(locs: List[(Int, Int)], acc: List[(Int, Int)]): List[(Int, Int)] = {
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
    legalPlacements1(Board.allLocationsList, List())

  }

  // todo, eliminate the return
  private def legalPlacement(piece: Piece, loc: (Int, Int)): Boolean = {

    val (locRow, locCol) = loc

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
          return false // you can move this into the while loop eval if you use a var - only if perf warrants
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

  // todo, test this mechanism to see how much slower it is and record it for posterity
  /*private val visitedArray = Array.fill(Game.BOARD_SIZE)(Array.fill(Game.BOARD_SIZE)(false))*/

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
  private val directions = Array((-1, 0), (0, -1), (1, 0), (0, 1))

  private def getCountsArray = Array(0, 0, 0, 0, 0)

  private def getTryLoc(loc: (Int, Int), i: Int) = {
    val (offsetRow, offsetCol) = directions(i)
    val tryLoc = (loc._1 + offsetRow, loc._2 + offsetCol)
    tryLoc
  }

  // is this location occupied?
  private def isOccupied(loc: (Int, Int)): Boolean = layout(loc._1)(loc._2).occupied

  private def isOutOfBounds(loc: (Int, Int)) = {
    val i = loc._1
    val j = loc._2

    val inbounds = i >= 0 && i < layout.length && j >= 0 && j < layout(0).length
    !inbounds
  }

  // find largest connected component on the board
  // based on https://en.wikipedia.org/wiki/Connected-component_labeling#One_component_at_a_time
  // except not using a queue, but rather doing a recursive search (labelNeig
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
  // ExecutionTime: 30%, 6,719/s (expected Time(ms) to go up as a % given other optimizations but strangey, per second regressed)
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
  private def findLargestConnectedComponent(layout: Array[Array[Cell]], locations: List[(Int, Int)]): Int = {

    //println("finding largest connected component.....................")
    // call dfs in all directions - only acting on valid locations - until it can't find any more islands
    val labels = connectedCountLabelsArray
    var componentCount = 0

    def getComponentCount(label: Int): Int = {
      /*island.map(a => a.count(_ == true)).sum*/

      @annotation.tailrec
      def componentCountLoop(row: Int, col: Int, acc: Int): Int = {

        //val countMe = (r:Int, c:Int) => if (island(r)(c)) 1 else 0

        (row, col) match {
          case (-1, _) => acc
          case (_, 0) => componentCountLoop(row - 1, labelsLength - 1, acc + (if (labels(row)(col) == label) 1 else 0))
          case _ => componentCountLoop(row, col - 1, acc + (if (labels(row)(col) == label) 1 else 0))
        }
      }

      componentCountLoop(labelsLength - 1, labelsLength - 1, 0)

    }

    // is it safe to recurse into this location
    def isSafe(loc: (Int, Int)): Boolean = {

      // has this location been visited?
      def isVisited(loc: (Int, Int)): Boolean = labels(loc._1)(loc._2) > 0

      if (isOutOfBounds(loc))
        return false

      if (isVisited(loc))
        return false

      if (isOccupied(loc))
        return false

      true

    }

    def labelLocAndNeighbors(loc: (Int, Int), currentLabel: Int): Unit = {

      label(loc, currentLabel)

      var i = 0
      val length = directions.length
      while (i < length) {
        val tryLoc: (Int, Int) = getTryLoc(loc, i)
        if (isSafe(tryLoc)) {
          labelLocAndNeighbors(tryLoc, currentLabel)
        }

        i += 1
      }

    }

    def label(loc: (Int, Int), label: Int): Unit = {
      labels(loc._1)(loc._2) = label
      //println("I just  labelled " + loc + " with: " + label)
    }

    def findLargestLoop(locs: List[(Int, Int)], largestComponent: Int): Int = {

      def processComponent(loc: (Int, Int), currentLabel: Int): Int = {
        labelLocAndNeighbors(loc, currentLabel)
        val currentCount = getComponentCount(currentLabel)
        if (currentCount > largestComponent) currentCount else largestComponent
      }

      locs match {
        case head :: tail => {

          if (isSafe(head)) {
            componentCount += 1
            val max: Int = processComponent(head, componentCount)
            findLargestLoop(tail, max)

          } else {

            findLargestLoop(tail, largestComponent)
          }
        }
        case Nil => largestComponent
      }

    }

    findLargestLoop(locations, 0)
  }

  private def countNeighbors(locs: Array[(Int, Int)]): Array[Int] = {

    val counts = getCountsArray
    val locLength = locs.length

    def hasNeighbor(loc: (Int, Int)): Boolean = isOutOfBounds(loc) || isOccupied(loc)
    // walk through all directions
    def countLocationNeighbor(loc: (Int, Int)): Unit = {

      val directionLength = directions.length
      var i = 0
      var count = 0
      while (i < directionLength) {
        val tryLoc = getTryLoc(loc, i)

        if (hasNeighbor(tryLoc))
          count += 1

        i += 1
      }

      counts(count) += 1

    }

    def countNeighbors1 = {
      var i = 0

      while (i < locLength) {
        val loc = locs(i)
        if (isOccupied(loc))
          ()
        else {
          countLocationNeighbor(loc)
        }

        i += 1
      }
    }

    countNeighbors1

    counts

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
        case (_, 0) => loop(row - 1, boardSize - 1, (row, col) :: acc)
        case _ => loop(row, col - 1, (row, col) :: acc)
      }
    }

    val size = boardSize
    loop(size - 1, size - 1, List())

  }

  lazy val allLocationsList: List[(Int, Int)] = getLocations(BOARD_SIZE)
  lazy val allLocations: Array[(Int, Int)] = allLocationsList.toArray

  private val BOARD_COLOR = Game.BRIGHT_WHITE
  private def layoutTemplate: Array[Array[Cell]] = Array(null, null, null, null, null, null, null, null, null, null)

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
    val newLayout = layoutTemplate // Array.ofDim[Array[Cell]](Game.BOARD_SIZE)
    var i = 0
    while (i < BOARD_SIZE) {
      newLayout(i) = boardToCopy.layout(i).clone
      i += 1
    }

    /* new Board(layout, newName)*/
    new Board(newLayout, newName)

  }
}