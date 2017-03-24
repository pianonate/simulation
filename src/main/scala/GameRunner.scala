/**
 * Created by nathan on 12/20/16.
 * let Main be Main
 * GameRunner owns the responsibility of running Games and managing high score persistence
 */

import Implicits._
import java.awt.Toolkit
import java.io.PrintWriter
import scala.collection.mutable.ListBuffer

class GameRunner extends Output {

  private val allGamesTimer = new GameTimer

  private var mockOutput = false
  private var simulateNewHighScore = false

  def testOnlySetMockOutput:Unit = mockOutput = true
  def testOnlySimulateNewHighScore:Unit = simulateNewHighScore = true

  def play(context: Context, startGameCount: Int = 0): Array[Int] = {

    context.logger.info("starting simulation")

    val scores = new ListBuffer[Int]
    val rounds = new ListBuffer[Int]
    val simulationsPerSecond = new ListBuffer[Int]
    val gameCount = Counter(startGameCount)

    def logGame(results: GameResults) {

      val roundsPerSecond: Double = (results.rounds / results.gameTimer.elapsedSeconds).toDouble

      context.logger.info("game " + gameCount.label(4).green
        + " - score: " + results.score.label(9).green
        + " average: " + scores.avg.toInt.label(9).green
        + " high score: " + scores.max.label(9).green
        + " rounds: " + results.rounds.label(7).green
        + " rounds/s: " + roundsPerSecond.label(2, 2).green
        + " duration: " + results.gameTimer.elapsedLabel.green
        + " game seed: " + results.gameSeed.rightAligned(10).green)

    }

    var continue = true

    // run the game, my friend
    do {

      val machineHighScore = getHighScore
      val sessionHighScore = if (scores.isEmpty) 0 else scores.max
      val average = if (scores.isEmpty) 0 else scores.avg.toInt

      gameCount.inc()

      val gameInfo = MultiGameStats(average, sessionHighScore, machineHighScore, gameCount.value, allGamesTimer)


       val game = if (mockOutput) {
         new Game(context, gameInfo) with MockOutput
       } else {
         new Game(context, gameInfo)
       }

      val results: GameResults = game.run

      scores.append(results.score)
      rounds.append(results.rounds)
      simulationsPerSecond.append(results.bestPerSecond)

      val potentialAllTimeHighScore = machineHighScore.max(scores.max)
      val mostRounds = rounds.max
      val bestPerSecond = simulationsPerSecond.max

      logGame(results)

      val endGameString = "multiple game stats".header + "\n" +
        "games played".label + gameCount.label + "\n" +
        "average score".label + scores.avg.toInt.scoreLabel + "\n" +
        "session high score".label + scores.max.scoreLabel + "\n" +
        "all time high score".label + potentialAllTimeHighScore.scoreLabel + "\n" +
        "most rounds".label + mostRounds.label + "\n" +
        "most simulations/s".label + bestPerSecond.label + "\n" +
        "total elapsed time".label + allGamesTimer.elapsedLabel + "\n\n"

      if (context.show)
        print(endGameString)

      if (potentialAllTimeHighScore > machineHighScore || simulateNewHighScore) {
        saveHighScore(potentialAllTimeHighScore)

        if (context.show)
          print("\n" + "new high score!!!!".greenHeader + "\n")

        if (context.stopAtNewHighScore)
          context.gamesToPlay = gameCount.value

      }

      continue = context.gamesToPlay == 0 || gameCount.value < context.gamesToPlay

      countDown(continue, context)

    } while (continue)

    // return value to the weighted game - probably better would be to create a routine that both
    // of these call to run a game - Play would be in a loop and weighted is "just one"
    // they have different logging etc. so there are differences that can be refactored
    scores.toArray

  }

  // used to beep at the end of the game
  private val toolKit: Toolkit = java.awt.Toolkit.getDefaultToolkit

  private def countDown(continue: Boolean, context: Context) = {
    if (continue && context.show) {

      print("\nstarting new game")

      if (context.beep) {
        print(" in ")
        // countdown timer
        (1 to 10).reverse.foreach { i =>
          print(i + "...")
          Console.out.flush()
          toolKit.beep()
          Thread.sleep(500)
        }
      } else {
        print(" immediately")
      }

      print("\nGo!\n")
      Console.out.flush()

    }
  }

  private def saveHighScore(highScore: Int): Unit = {
    val pw = new PrintWriter(Context.FILE_HIGH_SCORE)
    pw.write(highScore.toString)
    pw.close()

  }

  private def getHighScore: Int = {

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
