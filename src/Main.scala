/**
 * Created by nathan on 12/9/16.
 * Main is just the stub to get it all off the ground
 */


object Main extends App {

  // run the game, my friend
  Game.run()

//  printPossiblePieces
//  printPossibleColors


  private def printPossiblePieces(): Unit = {
    for (piece <- Piece.pieces) {
      println(piece.name + ": " + piece.pointValue)
      println(piece.toString)
    }
    println
  }

  // print the character colors that we have available to us
  private def printPossibleColors(): Unit = {
    for (i <- 30 to 37) {
      val code = i.toString
      print(f"\u001b[38;5;$code%sm$code%3s")
    }

    println("")

    for (i <- 90 to 97) {
      val code = i.toString
      print(f"\u001b" +
        f"[38;5;$code%sm$code%3s")
    }

    println

  }


}

