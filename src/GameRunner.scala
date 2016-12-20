/**
 * Created by nathan on 12/20/16.
 * let Main be Main
 * GameRunner owns the responsibility of running Games and managing high score persistence
 */
object GameRunner {

  def play(): Unit = {

    // todo - stash high scores in a file you can read at startup of the game
    // high score to date is 7,766

    // different than game continuous mode which simply
    // controls whether you hit enter to place the next piece
    // Main continuous mode means continuous play - you have to ctrl-c out of it
    val CONTINUOUS_MODE = true

    import scala.collection.mutable.ListBuffer
    val scores = new ListBuffer[Long]
    val rounds = new ListBuffer[Long]
    val simulationsPerSecond = new ListBuffer[Long]

    Game.showGameStart()

    // run the game, my friend
    do {
      val game = new Game()
      val results = game.run()

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

        println
        print("Starting new game in ")

        // countdown timer
        (1 to 10).reverse.foreach { i =>
          print(i + "...")
          Thread.sleep(1000)
        }

        println
        println("Go!")
        println
      }

    } while (CONTINUOUS_MODE)

  }

}
