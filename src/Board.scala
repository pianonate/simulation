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
  override val weight = 0

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

  private def islands: List[Int] = Islands.findIslands(this.layout, Board.allLocations)

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

  // next level of optimization.
  // start: clearLines takes 58% of placeMe at 2,992 per second in the profiler
  // fullRow and fullCol eliminate forall:  now 35% of placeMe at 5,344 per second in the profiler
  // fullRows and fullCols built with tail recursion: now 4% of placeMe at 89,635 / second in the profiler
  // final clearing removed foreach on clearableRows and clearableCols:  3% of placeMe at 104,000 / second in the profiler
  // overall improvement = 3300%
  //
  // at start of this optimization, clearLines was 11% of overall execution time at end, it was .3%
  // this is a super strong argument for using while loops and buildings things tail recursively when performance
  // is on the line in tight loops
  //
  // todo: att tailrec annotation

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

    // todo - get rid of while loop duplication in general.  You can pass in functions to do the right thing
    //        also have a while loop elimination that allows for early exit - which might just be you provide the
    //        test situation as a lambda
    def fullRow(row: Array[Cell]): Boolean = { //row.forall(cell => cell.occupied)

      var i = 0

      // if you find any unoccupied then the row can't be full
      var unoccupied = false

      // replaced a return when unoccupied with a conditional that evaluates for it
      // returns are bad in scala
      // https://tpolecat.github.io/2014/05/09/return.html
      while ((i < row.length) && !unoccupied) {
        if (row(i).unoccupied) {
          unoccupied = true
        }
        i += 1
      }

      if (unoccupied) // if a cell is unoccupied then we can't be full
        false
      else
        true
    }

    def fullCol(col: Int): Boolean = { //layout.forall(row => row(col).occupied)
      var i = 0

      // if you find any unoccupied then the row can't be full
      var unoccupied = false

      while ((i < layout.length) & !unoccupied) {
        if (!layout(i)(col).occupied) {
          unoccupied = true
        }
        i += 1
      }

      if (unoccupied) // if a cell is unoccupied then we can't be full
        false
      else
        true

    }

    def fullRows(): Seq[Int] = {

      @tailrec def fullRowLoop(index: Int, acc: List[Int]): List[Int] = {
        index match {
          case n if n < layout.length =>
            if (fullRow(layout(n)))
              fullRowLoop(n + 1, n :: acc)
            else
              fullRowLoop(n + 1, acc)
          case _ => acc
        }

      }

      fullRowLoop(0, List())
    }

    def fullCols(): Seq[Int] = {

      @tailrec def fullColLoop(index: Int, acc: List[Int]): List[Int] = {
        index match {
          case n if n < layout.length =>
            if (fullCol(n))
              fullColLoop(n + 1, n :: acc)
            else
              fullColLoop(n + 1, acc)
          case _ => acc
        }

      }

      fullColLoop(0, List())
    }

    val clearableRows = fullRows()
    val clearableCols = fullCols()

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

  def place(piece: Piece, loc: (Int, Int)): Unit = {

    // walk through every occupied location in the piece
    // and map it to the appropriate location on the underlying board
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
    /*for { loc <- Board.allLocationsList if legalPlacement(piece, loc) } yield loc*/

    @tailrec def loop(locs: List[(Int, Int)], acc: List[(Int, Int)]): List[(Int, Int)] = {
      locs match {
        case head :: tail =>
          if (legalPlacement(piece, head))
            loop(tail, head :: acc)
          else
            loop(tail, acc)
        case Nil => acc
      }
    }

    loop(Board.allLocationsList, List())

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

    @tailrec def loop(row: Int, col: Int, acc: List[(Int, Int)]): List[(Int, Int)] = {
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
  lazy val allLocationsList: List[(Int, Int)] = allLocations.toList

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