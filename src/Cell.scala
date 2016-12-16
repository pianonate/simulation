/**
 * Created by nathan on 12/9/16.
 * a cell within a piece is either occupied or not.  it also has a color.
 * if it's the game board piece, then it should show unoccupied cells on toString
 */
case class Cell(occupied: Boolean = false, color: String, var underline:Boolean = false) {

  // Todo: get rid of val - i think you could just create a copy of the cell.
  val underLineColor = GameUtil.UNDERLINE + color

  override def toString: String = {
    if (occupied)
      if (underline) underLineColor + Cell.BOX_CHAR else color + Cell.BOX_CHAR
    else
      Cell.unoccupiedColor + Cell.BOX_CHAR
  }

}

object Cell {
  private val BOX_CHAR = "\u25A0" + GameUtil.SANE
  private val unoccupiedColor = GameUtil.BRIGHT_WHITE
  def copy(cell:Cell):Cell = new Cell(cell.occupied, cell.color, cell.underline)

}