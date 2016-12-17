/**
 * Created by nathan on 12/9/16.
 * Main is just the stub to get it all off the ground
 */


object Main extends App {

  // todo - stash high scores in a file you can read at startup of the game
  // high score to date is 2,2656

  // different than game continuous mode which simply
  // controls whether you hit enter to place the next piece
  // Main continuous mode means continuous play - you have to ctrl-c out of it
  val CONTINUOUS_MODE = true

  val scores = new scala.collection.mutable.ListBuffer[Long]
  val rounds = new scala.collection.mutable.ListBuffer[Long]

  // run the game, my friend
  do {
    val game = new Game()
    val score = game.run
    scores.append(score._1)
    rounds.append(score._2)


    if (CONTINUOUS_MODE) {
        val highScore = scores.max
        val mostRounds = rounds.max
        println
        println
        println("High Score:   " + "%,7d".format(highScore))
        println("Games Played: " + "%,7d".format(scores.size))
        println("Most Rounds:  " + "%,7d".format(mostRounds))

      // todo: make it idiomatic
        println
        print("Starting new game in 5...")
        Thread.sleep(1000)
        print("4..")
        Thread.sleep(1000)
        print("3..")
        Thread.sleep(1000)
        print("2..")
        Thread.sleep(1000)
        print("1..")
        Thread.sleep(1000)
        println
        println("Go!")
        println
      }

  } while (CONTINUOUS_MODE)

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

