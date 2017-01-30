/**
 * Created by nathan on 12/20/16.
 * let Main be Main
 * GameRunner owns the responsibility of running Games and managing high score persistence
 */
import java.awt.Toolkit

import Game._
import Implicits._
import java.io.PrintWriter

object GameRunner {

  val toolKit: Toolkit = java.awt.Toolkit.getDefaultToolkit

  def play(context: Context): Unit = {

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
      val average = if (scores.isEmpty) 0 else scores.avg.toInt

      gameCount.inc()

      val gameInfo = MultiGameStats(average, sessionHighScore, machineHighScore, gameCount.value, totalTime)

      val game = new Game(context, gameInfo)
      val results = game.run

      scores.append(results.score)
      rounds.append(results.rounds)
      simulationsPerSecond.append(results.bestPerSecond)

      val allTimeHighScore = List(machineHighScore, scores.max).max
      val mostRounds = rounds.max
      val bestPerSecond = simulationsPerSecond.max

      val endGameString = "multiple game stats".header + "\n" +
        "games played".label + gameCount.label + "\n" +
        "average score".label + scores.avg.toInt.scoreLabel + "\n" +
        "session high score".label + scores.max.scoreLabel + "\n" +
        "all time high score".label + allTimeHighScore.scoreLabel + "\n" +
        "most rounds".label + mostRounds.label + "\n" +
        "most simulations/s".label + bestPerSecond.label + "\n" +
        "total elapsed time".label + totalTime.elapsedLabel + "\n\n"

      if (context.show)
        print(endGameString)

      if (allTimeHighScore > machineHighScore) {
        saveHighScore(allTimeHighScore)

        if (context.show)
          print("\n" + "new high score!!!!".greenHeader + "\n")

        if (context.stopAtNewHighScore)
          context.continuousMode = false

      }

      countDown(context)

    } while (context.continuousMode)

  }

  private def countDown(context: Context) = {
    if (context.continuousMode && context.show) {

      print("\nstarting new game in ")

      // countdown timer
      (1 to 10).reverse.foreach { i =>
        print(i + "...")
        beep(context)
        Thread.sleep(500)
      }

      print("\nGo!\n")

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

  def beep(context: Context): Unit = if (context.beep) toolKit.beep()

}
