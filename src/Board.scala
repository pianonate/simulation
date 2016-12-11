/**
 * Created by nathan on 12/9/16.
 * Board is the game Board plus helper functions.
 * Boards will be created at will to test out different combinations.
 * THere will always be the main board (probably need a Game to hold that plus Boards used for simulations to try out different algos
 * So a copy method will probably also be needed by that point
 */
class Board(size: Int) {

  // Pieces are all created with the factory method of the Piece object
  // the Board is a special kind of Piece - one that is initialized completely empty
  private val board:Piece = new BoardPiece(size)

  override def toString:String = board.toString

}
