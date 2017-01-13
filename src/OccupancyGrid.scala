/**
 * Created by nathan on 1/11/17.
 * my attempt to use bit operations to speed up this damn thing
 * which definitely worked for open lines and max contiguous lines
 */
case class OccupancyGrid(
  rows:          Int,
  cols:          Int,
  rowGrid:       Array[Int],
  colGrid:       Array[Int],
  occupancyGrid: Array[Array[Boolean]]
) {

  import OccupancyGrid._

  override def toString: String = {

    rowGrid.zipWithIndex.map { row =>

      val seq = (0 until cols) map { x => getBitAt(row._2, x) }

      "[" + seq.mkString + "]"
    }.mkString("\n")

  }

  def getOccupancyGrid: Array[Array[Boolean]] = occupancyGrid

  def copy: OccupancyGrid = {

    // using clone vs. map reduced % of total execution time spent in .copy from 18% to 2%
    val newRowGrid = rowGrid.clone // rowGrid.map(identity)
    val newColGrid = colGrid.clone // colGrid.map(identity)

    val newOccupancyGrid = {

      val newOccupancyGrid = new Array[Array[Boolean]](occupancyGrid.length)

      // again, substantially faster to clone than any other way
      var i = 0
      while (i < occupancyGrid.length) {
        newOccupancyGrid(i) = occupancyGrid(i).clone
        i += 1
      }
      newOccupancyGrid
    }

    OccupancyGrid(rows, cols, newRowGrid, newColGrid, newOccupancyGrid)

  }

  /**
   * rotate both transposes and reverses columns
   * it's enough to know it's figured out for this one off case
   * don't care much about a generic answer right now
   */
  def rotate: OccupancyGrid = {

    def rotateImpl(rotateMe: Array[Int]): Array[Int] = {

      val newGrid = new Array[Int](cols)

      val iOffsetStart = 0
      val jOffsetStart = rows - 1

      var iOffset = iOffsetStart
      var jOffset = jOffsetStart

      var i = 0
      var j = 0

      while (i < rows) {
        while (j < cols) {

          val value = getBitAt(rotateMe, i, j)

          if (value == 1) {
            newGrid(i + iOffset) |= (value << (j + jOffset))
          }

          iOffset += 1
          jOffset -= 1
          j += 1
        }

        j = 0
        i += 1
        iOffset = iOffsetStart - i
        jOffset = jOffsetStart - i

      }

      newGrid
    }

    val newRowGrid = rotateImpl(rowGrid)
    val newColGrid = rotateImpl(colGrid)

    val newOccupancyGrid = occupancyGrid.transpose.map(_.reverse)

    OccupancyGrid(cols, rows, newRowGrid, newColGrid, newOccupancyGrid)

    /*
    for {
      i <- rows
      j <- cols
    } {

      if occupied(i,j)

        occupy(i, j+2)
        occupy(i+1,j+1)
        occupy(i+2,j+0)

        occupy(i-1, j+1)
        occupy(i +0, j+0)
        occupy(i+1, j-1)

        occupy(i-2, j)
        occupy(i-1, j-1)
        occupy(i-0, j-2)

      else
        unoccupy(i, j+2)

      100 (0,2) (1,2) (2,2)
      100 (0,1) (1,1) (2,1)
      111 (0,0) (1,0) (2,0)

      111
      100
      100

      111 (0,0) (1,0) (2,0)

      1
      1
      1
*/
  }

  def clearRow(row: Int): Unit = {
    var col = 0
    while (col < cols) {
      unSetBitAt(colGrid, col, row)
      occupancyGrid(row)(col) = false
      col += 1
    }
    // it's an optimization to not just use unoccupy
    // as we can zero out the rowGrid in one fell swoop
    rowGrid(row) = zero
  }

  def clearCol(col: Int): Unit = {
    var row = 0
    while (row < rows) {
      unSetBitAt(rowGrid, row, col)
      occupancyGrid(row)(col) = false
      row += 1
    }
    // it's an optimization to not just use unoccupy
    // as we can zero out the colGrid in one fell swoop
    colGrid(col) = zero
  }

  def openLineCount: Int = getOpenLineCount(rowGrid) + getOpenLineCount(colGrid)

  private def getOpenLineCount(theGrid: Array[Int]): Int = {
    var i = 0
    var count = 0
    while (i < theGrid.length) {
      if (theGrid(i) == zero) { count += 1 }
      i += 1
    }
    count
  }

  def maxContiguousOpenLines: Int = {

    def getMaxContiguous(theGrid: Array[Int]): Int = {
      var i = 0
      var max = 0
      var currentMax = 0
      while (i < theGrid.length) {
        if (theGrid(i) == zero) {
          currentMax += 1
          if (currentMax > max) { max = currentMax }
        } else {
          currentMax = 0
        }
        i += 1
      }
      max
    }

    val maxRows = getMaxContiguous(rowGrid)
    val maxCols = getMaxContiguous(colGrid)

    if (maxRows > maxCols) maxRows else maxCols

  }

  private def getFullLine(theGrid: Array[Int]): Array[Int] = {
    // it's a hack to pass in sentinel values (-1's from the fullLineArray
    // but it prevents us from doing a splitAt (or just constructing this thing functionally
    // doing it this way is orders of magnitude faster
    val a = getFullLineArray
    var i = 0
    var n = 0
    while (i < a.length) {
      if (theGrid(i) == fullLineValue) {
        a(n) = i
        n += 1
      }
      i += 1
    }
    a
  }

  def fullRows: Array[Int] = getFullLine(rowGrid)
  def fullCols: Array[Int] = getFullLine(colGrid)

  def occupied(row: Int, col: Int): Boolean = occupancyGrid(row)(col)

  private def getBitAt(theGrid: Array[Int], row: Int, col: Int): Int = (theGrid(row) >> col) & 1
  private def getBitAt(row: Int, col: Int): Int = getBitAt(rowGrid, row, col)

  private def setBitAt(theGrid: Array[Int], row: Int, col: Int) = theGrid(row) |= (1 << col)
  private def unSetBitAt(theGrid: Array[Int], row: Int, col: Int) = theGrid(row) &= ~(1 << col)

  /**
   * set occupancy to true at this position
   */
  def occupy(row: Int, col: Int): Unit = {
    setBitAt(rowGrid, row, col)
    setBitAt(colGrid, col, row)
    occupancyGrid(row)(col) = true
  }

  /**
   * set occupancy to false at this position
   */
  def unoccupy(row: Int, col: Int): Unit = {
    unSetBitAt(rowGrid, row, col)
    unSetBitAt(colGrid, col, row)
    occupancyGrid(row)(col) = false
  }

}

object OccupancyGrid {

  private val zero = 0
  private val fullLineValue = math.pow(2, Board.BOARD_SIZE).toInt - 1
  private val fullLineArray = Array.fill[Int](Board.BOARD_SIZE)(-1)
  private def getFullLineArray = fullLineArray.clone

  private def getNewGrid(rows: Int, cols: Int, fillMe: Boolean): Array[Int] =
    if (fillMe) Array.ofDim[Int](rows).map(_ => fullLineValue) else new Array[Int](rows)

  private def getNewOccupancyGrid(rows: Int, cols: Int, fillMe: Boolean): Array[Array[Boolean]] =
    if (fillMe) Array.fill(rows, cols)(true) else Array.ofDim[Boolean](rows, cols)

  def apply(rows: Int, cols: Int, filled: Boolean): OccupancyGrid = OccupancyGrid(
    rows,
    cols,
    OccupancyGrid.getNewGrid(rows, cols, filled),
    OccupancyGrid.getNewGrid(rows, cols, filled),
    OccupancyGrid.getNewOccupancyGrid(rows, cols, filled)
  )

}