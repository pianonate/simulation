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

  import scala.collection.mutable.ListBuffer
  val scores = new ListBuffer[Long]
  val rounds = new ListBuffer[Long]
  val simulationsPerSecond = new ListBuffer[Long]

  Game.showGameStart

  // run the game, my friend
  do {
    val game = new Game()
    val results = game.run

    scores.append(results._1)
    rounds.append(results._2)
    simulationsPerSecond.append(results._3)

    if (CONTINUOUS_MODE) {
      val highScore = scores.max
      val mostRounds = rounds.max
      val bestPerSecond = simulationsPerSecond.max

      val labelFormat = GameUtil.labelFormat
      val numberFormat = GameUtil.numberFormat

      println
      println
      println("MULTIPLE GAME STATS")
      println
      println(labelFormat.format("Games Played") + numberFormat.format(scores.size))
      println(labelFormat.format("High Score") + GameUtil.RED + numberFormat.format(highScore) + GameUtil.SANE)
      println(labelFormat.format("Most Rounds") + numberFormat.format(mostRounds))
      println(labelFormat.format("Most Simulations/Second") + numberFormat.format(bestPerSecond))

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
    val pieces = (new Pieces).pieceList
    for (piece <- pieces) {
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

