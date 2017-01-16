/**
 * Created by nathan on 12/9/16.
 * a cell within a piece is either occupied or not.  it also has a color.
 * if it's the game board piece, then it should show unoccupied cells .show
 */
case class Cell(occupied: Boolean = false, color: String) {

  val unoccupied: Boolean = !occupied

  var shown = false

  override def toString: String = if (this.occupied) "occupied" else "unoccupied"

  def showForPiece: String = {
    if (occupied)
      color + Cell.BOX_CHAR + Game.SANE
    else
      Cell.unoccupiedColor + Cell.BOX_CHAR + Game.SANE
  }

  def showForBoard: String = {

    if (occupied)
      if (!shown) {
        shown = true
        Game.UNDERLINE + color + Cell.BOX_CHAR + Game.SANE
      }
      else
        color + Cell.BOX_CHAR + Game.SANE
    else
      Cell.unoccupiedColor + Cell.BOX_CHAR + Game.SANE

  }

}

object Cell {

  private val BOX_CHAR = "\u25A0"
  private val unoccupiedColor = Game.BRIGHT_WHITE
  def copy(cell: Cell): Cell = new Cell(cell.occupied, cell.color)

}