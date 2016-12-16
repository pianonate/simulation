/**
 * Created by nathan on 12/9/16.
 * a cell within a piece is either occupied or not.  it also has a color.
 * if it's the game board piece, then it should show unoccupied cells on toString
 */
case class Cell(occupied: Boolean = false, color: Ansi, var underline:Boolean = false) {

  // Todo: get rid of val - i think you could just create a copy of the cell.
  val underLineColor = Ansi.Underline and color

  override def toString: String = {
    if (occupied)
      if (underline) underLineColor.format(Cell.BOX_CHAR) else color.format(Cell.BOX_CHAR)
    else
      Cell.unoccupiedColor.format(Cell.BOX_CHAR)
  }

}

object Cell {
  private val BOX_CHAR = "\u25A0"
  private val unoccupiedColor = Ansi.BrightWhite
  def copy(cell:Cell):Cell = new Cell(cell.occupied, cell.color, cell.underline)

}