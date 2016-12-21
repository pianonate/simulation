/**
 * Created by nathan on 12/9/16.
 * Main is just the stub to get it all off the ground
 */

import scala.collection.mutable.ListBuffer

object Main extends App {

  GameRunner.play()
  /*
  // attempt to rewrite the recursion on the board
  // helpful to get this done before trying anything more crazy as the current set up is too fragile

  val board = new Board(10)
  val gamePieces = new Pieces
  val pieces = List.fill(3)(gamePieces.getRandomPiece)

  def recurse(pieces: List[Piece], board: Board): Board = {
    // place the head
    // pass the next piece to recurse

    def placeMe(piece:Piece, loc:(Int, Int), board:Board) = {
      board.place(piece, loc)
      board.clearLines()
    }

    pieces match {
      case head::tail => placeMe()
      case Nil        => board
    }

  }*/

}

