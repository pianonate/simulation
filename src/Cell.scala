/**
 * Created by nathan on 12/9/16.
 * a cell within a piece is either occupied or not.  it also has a color.
 * if it's the game board piece, then it should show unoccupied cells on toString
 */
class Cell ( val occupied: Boolean = false, val color: Ansi = Cell.unoccupiedColor) {

  override def toString:String = {
    if (occupied)
     color.format(Cell.BOX_CHAR)
    else
      Cell.unoccupiedColor.format(Cell.BOX_CHAR)
  }

}

object Cell {
  private val BOX_CHAR = "\u25A0"
  private val unoccupiedColor = Ansi.BrightWhite
}