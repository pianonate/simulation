/**
 * Created by nathan on 12/9/16.
 */
class Board (size: Int) {

  private val b = Array.ofDim[Int](size,size)

  private val board = Array.ofDim[Cell](size,size)
  for {i <- 0 until size
    j <- 0 until size}
    board(i)(j) = new Cell

  override def toString = {

    val s = new StringBuilder()
    for (row <- board) {
      s ++= row.map(Board.cellAsString).foldRight(" ")((a, b) => a + " " + b) + "\n"
    }

    s.toString
  }




}

object Board {

  private val BOX = '\u25A0'
  private def cellAsString(cell: Cell) : String = {
    cell.color + BOX
  }

}
