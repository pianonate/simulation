/**
 * Created by nathan on 12/20/16.
 * let Main be Main
 * GameRunner owns the responsibility of running Games and managing high score persistence
 */
import Game._
import java.io.PrintWriter

object GameRunner {

  def play(context:Context): Unit = {

    import scala.collection.mutable.ListBuffer

    val scores = new ListBuffer[Int]
    val rounds = new ListBuffer[Int]
    val simulationsPerSecond = new ListBuffer[Int]
    val gameCount = Counter()
    val totalTime = new GameTimer

    Game.showGameStart(context.specification)

    // run the game, my friend
    do {

      val machineHighScore = getHighScore
      val sessionHighScore = if (scores.isEmpty) 0 else scores.max

      gameCount.inc()

      val gameInfo = GameInfo(sessionHighScore, machineHighScore, gameCount.value, totalTime )

      val game = new Game(context, gameInfo)
      val results = game.run

      scores.append(results.score)
      rounds.append(results.rounds)
      simulationsPerSecond.append(results.bestPerSecond)

      val allTimeHighScore = List(machineHighScore, scores.max).max
      val mostRounds = rounds.max
      val bestPerSecond = simulationsPerSecond.max

      println

      println(Game.getHeaderString("multiple game stats"))
      println(labelFormat.format("games played") + numberFormat.format(gameCount.value))
      println(labelFormat.format("session high score") + getScoreString(numberFormat, scores.max))
      println(labelFormat.format("all time high score") + getScoreString(numberFormat, allTimeHighScore))
      println(labelFormat.format("most rounds") + numberFormat.format(mostRounds))
      println(labelFormat.format("most simulations/s") + numberFormat.format(bestPerSecond))
      println(labelFormat.format("total elapsed time") +  elapsedFormat.format(totalTime.showElapsed))

      if (allTimeHighScore > machineHighScore) {
        saveHighScore(allTimeHighScore)
        context.continuousMode = false
        println(Game.getHeaderString(Game.GREEN, "new high score!!!!"))

      }

      countDown(context)

    } while (context.continuousMode)

  }

  private def countDown(context: Context) = {
    if (context.continuousMode) {
      println
      print("starting new game in ")

      // countdown timer
      (1 to 10).reverse.foreach { i =>
        print(i + "...")
        Thread.sleep(500)
      }

      println
      println("Go!")
      println
    }
  }

  def saveHighScore(highScore: Int): Unit = {
    val pw = new PrintWriter(Context.FILE_HIGH_SCORE)
    pw.write(highScore.toString)
    pw.close()

  }

  def getHighScore: Int = {

    import scala.io.Source

    try {
      return Source.fromFile(Context.FILE_HIGH_SCORE).getLines.mkString.toInt

    } catch {
      case _: Throwable =>
        val pw = new PrintWriter(Context.FILE_HIGH_SCORE)
        pw.write("0")
        pw.close()
    }

    0

  }

}
