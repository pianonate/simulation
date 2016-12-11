/**
 * Created by nathan on 12/10/16.
 * Game will hold a reference to the current board and will also invoke simulations
 */
class Game {

  private val board = new Board(10)

  def run = {
    println(board.toString)
  }

}
