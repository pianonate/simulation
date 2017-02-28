/**
 * Created by nathan on 12/20/16.
 * let Main be Main
 * GameRunner owns the responsibility of running Games and managing high score persistence
 */

import Implicits._
import java.awt.Toolkit
import java.io.PrintWriter

object GameRunner {

  def generateWeights(context: Context): Unit = {

    context.logger.info("generating weights")

    val allGamesTimer = new GameTimer

    def gamesPerMinute(games: Int) = math.floor(games / allGamesTimer.elapsedMinutes).toInt

    context.gamesToPlay = 1

    val specification = Specification(filtered = false)
    val iterations = context.generateWeightsGamesToPlay
    val totalGames = iterations * specification.length
    val longestKeyLength = specification.spec.keys.map(_.length).max

    // play the same game for each
    val randomizer = new scala.util.Random(1)

    // todo - ask kevin
    // i thought of an optimization to store previously generated weights in a file
    // given that the randomization is always seeded with the same value (above)
    // however because the game plays multi-threaded, it may be that a particular set
    // of locs are chosen for a round that is different each time hou run it
    // this is because different locations will result in the same weight value
    // even if one is obviously better than another - they're not different when it comes to
    // that one weight value
    // do it doesn't seem legitimate to choose one result vs. another by simply
    // favoring the run that you wrote to disk.
    // check with Kevin or Brendan on this as maybe it doesn't make a difference

    val seeds = Array.fill[Int](iterations)(randomizer.nextInt.abs)

    val scores = specification.spec.zipWithIndex.map {
      case ((key, optFactor), factorIndex) =>

        context.specification = Specification(optFactor)

        val gameScore: Seq[Int] = for (gameIndex <- 0 until iterations) yield {
          val singleGameTimer = new GameTimer

          // each factor will play the same game to see how each performs against the same set of pieces
          context.setGameSeed(seeds(gameIndex))

          // play a game and get its score back - this seems a little obscure in terms of how to get the score...
          // could you be more clear?
          val score = play(context, gameIndex)(0)

          val completed: Int = (factorIndex * iterations) + (gameIndex + 1)

          val result = optFactor.key.rightAlignedPadded(longestKeyLength) + " - score: " + score.label(6).green +
            " - done in " + singleGameTimer.elapsedLabel.trim.leftAlignedPadded(6).green +
            "- game: " + completed.label(4) + " out of " + totalGames +
            " (" + ((completed.toDouble / totalGames) * 100).label(1).trim.green + "%".green + ")" +
            " game seed: " + context.getGameSeed.toString.green +
            " elapsed: " + allGamesTimer.elapsedLabelMs.trim.green

          // output to screen and to main log file
          println(result)
          context.logger.info(result)

          score
        }

        (key, gameScore.toArray)
    }

    println

    val sumOfAllGames = scores.values.map(game => game.sum).sum

    println("factors".label + specification.length.shortLabel)
    println("games per factor".label + iterations.shortLabel)
    println("games played".label + totalGames.shortLabel + " in " + allGamesTimer.elapsedLabel.trim)
    val endGamesPerMinute = gamesPerMinute(totalGames)

    println("games/minute".label + endGamesPerMinute)
    println("set iterations to: " + (endGamesPerMinute * 10 / specification.length) + " for 10 minutes of calculating")
    println("set iterations to: " + (endGamesPerMinute * 60 / specification.length) + " for 1 hour of calculating")

    println
    scores.toSeq.sortBy(a => a._2.sum * -1).foreach {
      case (key, sortedScores) =>

        val scoreSum = sortedScores.sum.toDouble
        println(key.leftAlignedPadded(Specification.maxOptFactorKeyLength + 1).appendColon + "average".appendColon + sortedScores.avg.toInt.label(6) + " - weight".appendColon + (scoreSum / sumOfAllGames))

    }

    println

    // todo - right now this is a bug waiting to happen (the 4 in repeat and label) - it needs to be based on the total number of games and what will be output because of that
    val header = "game" + scores.keys.toArray.map(key => " ".repeat(4) + key.rightAlignedPadded(Specification.maxOptFactorKeyLength)).mkString(" ") + "\n"

    val s = scores.values.transpose.zipWithIndex.map(game =>
      (game._2 + 1).label(4).toString + " " + game._1.map(each =>
        " ".repeat(Specification.maxOptFactorKeyLength - each.toString.length) + " ".repeat(3) + each)
        .mkString("  "))
      .mkString("\n")

    println(header + s)

    println
    println
    println("paste this code into object Specification")
    println

    val code = "private val weightMap = Map(\n\n" +
      scores.toSeq.sortBy(a => a._2.sum * -1).map({
        case (key, gameScores) =>
          val scoreSum = gameScores.sum.toDouble

          "\t\"" + key + "\" -> " + (scoreSum / sumOfAllGames)

      }).mkString(",\n").dropRight(2) + "\n\n" + ")"

    println(code)

  }

  def play(context: Context, startGameCount: Int = 0): Array[Int] = {

    if (!context.generatingWeights)
      context.logger.info("starting simulation")

    import scala.collection.mutable.ListBuffer

    val scores = new ListBuffer[Int]
    val rounds = new ListBuffer[Int]
    val simulationsPerSecond = new ListBuffer[Int]
    val gameCount = Counter(startGameCount)
    val totalTime = new GameTimer

    def logGame(results: GameResults) {

      if (!context.generatingWeights) {

        val roundsPerSecond = (results.rounds / results.gameTimer.elapsedSeconds).toDouble.label(2)

        context.logger.info("game " + gameCount.label(4).green
          + " - score: " + results.score.label(7).green
          + " average: " + scores.avg.toInt.label(7).green
          + " high score: " + scores.max.label(7).green
          + " rounds: " + results.rounds.label(7).green
          + " rounds/s: " + roundsPerSecond.green
          + " duration: " + results.gameTimer.elapsedLabel.green
          + " game seed: " + context.getGameSeed)
      }
    }

    var continue = true

    // run the game, my friend
    do {

      val machineHighScore = getHighScore
      val sessionHighScore = if (scores.isEmpty) 0 else scores.max
      val average = if (scores.isEmpty) 0 else scores.avg.toInt

      gameCount.inc()

      val gameInfo = MultiGameStats(average, sessionHighScore, machineHighScore, gameCount.value, totalTime)

      val game = new Game(context, gameInfo)

      val results: GameResults = game.run

      scores.append(results.score)
      rounds.append(results.rounds)
      simulationsPerSecond.append(results.bestPerSecond)

      val allTimeHighScore = List(machineHighScore, scores.max).max
      val mostRounds = rounds.max
      val bestPerSecond = simulationsPerSecond.max

      logGame(results)

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
