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

    val timer = new GameTimer

    def gamesPerMinute(games: Int) = math.floor(games / timer.elapsedMinutes).toInt

    context.gamesToPlay = 1

    val specification = Specification(filtered = false)
    val iterations = context.generateWeightsGamesToPlay
    val iterationLength = iterations.toString.length
    val totalGames = iterations * specification.length
    val longestKeyLength = specification.spec.keys.map(_.length).max

    // play the same game for each
    val randomizer = new scala.util.Random(1)

    // todo write randomizer output to a file as it won't change except when you change the algorithm of
    // a particular factor - you should load already calculated
    // weights from the file and then pick up wherever it left off and add more if a larger value was specified

    val seeds = Array.fill[Int](iterations)(randomizer.nextInt)

    val scores = specification.spec.zipWithIndex.map {
      case ((key, optFactor), factorIndex) =>

        context.specification = Specification(optFactor)

        val gameScore: Seq[Int] = for (gameIndex <- 0 until iterations) yield {
          val t = new GameTimer

          // each factor will play the same game to see how each performs against the same set of pieces
          context.setGameSeed(seeds(gameIndex))

          print(optFactor.key.rightAlignedPadded(longestKeyLength) + " game - " + (gameIndex + 1).label(iterationLength))

          // play a game and get its score back - this seems a little obscure in terms of how to get the score...
          // could you be more clear?
          val score = play(context, gameIndex)(0)

          val completed: Int = (factorIndex * iterations) + (gameIndex + 1)

          println(" - score: " + score.label(6) +
            " - done in " + t.elapsedLabel.trim.leftAlignedPadded(6) +
            "- game: " + completed.label(4) + " out of " + totalGames +
            " (" + ((completed.toDouble / totalGames) * 100).label(2).trim + "%)")

          score
        }

        (key, gameScore.toArray)
    }

    println

    val sumOfAllGames = scores.values.map(game => game.sum).sum

    println("factors".label + specification.length.shortLabel)
    println("games per factor".label + iterations.shortLabel)
    println("games played".label + totalGames.shortLabel + " in " + timer.elapsedLabel.trim)
    val endGamesPerMinute = gamesPerMinute(totalGames)

    println("games/minute".label + endGamesPerMinute)
    println("set iterations to: " + (endGamesPerMinute * 10 / specification.length) + " for 10 minutes of calculating")
    println("set iterations to: " + (endGamesPerMinute * 60 / specification.length) + " for 1 hour of calculating")

    println
    scores.toSeq.sortBy(a => a._2.sum * -1).foreach {
      case (key, sortedScores) =>

        val scoreSum = sortedScores.sum.toDouble
        println(key.leftAlignedPadded(Specification.maxOptFactorKeyLength + 1).addColon + "average".addColon + sortedScores.avg.toInt.label(6) + " - weight".addColon + (scoreSum / sumOfAllGames))

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
        case (key, scores) =>
          val scoreSum = scores.sum.toDouble

          ("\t\"" + key + "\" -> " + (scoreSum / sumOfAllGames))

      }).mkString(",\n").dropRight(2) + "\n\n" + ")"

    println(code)

  }

  def play(context: Context, startGameCount: Int = 0): Array[Int] = {

    import scala.collection.mutable.ListBuffer

    val scores = new ListBuffer[Int]
    val rounds = new ListBuffer[Int]
    val simulationsPerSecond = new ListBuffer[Int]
    val gameCount = Counter(startGameCount)
    val totalTime = new GameTimer

    var continue = true

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

      context.logger.info("game " + gameCount.shortLabel
        + " - score: " + results.score.scoreLabel
        + " average: " + scores.avg.toInt.scoreLabel
        + " high score: " + scores.max.scoreLabel
        + " duration: " + results.gameTimer.elapsedLabel)

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
      }
      else {
        print(" immediately")
      }

      print("\nGo!\n")
      Console.out.flush()

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
