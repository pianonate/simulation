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

  def generateWeights(context: Context): Unit = {

    val timer = new GameTimer

    def gamesPerMinute(games: Int) = math.floor(games / timer.elapsedMinutes).toInt

    context.gamesToPlay = 1

    val specification = Specification(filtered = false)
    val iterations = context.generateWeightsGamesToPlay
    val iterationLength = iterations.toString.length
    val totalGames = iterations * specification.length
    val longestKeyLength = specification.spec.keys.map(_.length).max


    // play the same g
    val seeds = Array.fill[Int](iterations)(scala.util.Random.nextInt)

    val scores = specification.spec.zipWithIndex.map {
      case ((key, optFactor), factorIndex) =>

        context.specification = Specification(optFactor)
        val scores: Seq[Int] = for (gameIndex <- 0 until iterations) yield {
          val t = new GameTimer

          // each factor will play the same game to see how each performs against the same set of pieces
          context.randomSeed = seeds(gameIndex)

          print(optFactor.key.rightAlignedPadded(longestKeyLength) + " game - " + (gameIndex + 1).label(iterationLength))

          val score = play(context, gameIndex)(0)

          val completed = (factorIndex * iterations) + (gameIndex + 1)

          println(" - score: " + score.label(6) +
            " - done in " + t.elapsedLabel.trim.leftAlignedPadded(6) +
            "- game: " + completed + " out of " + totalGames +
            " (" + ((completed.toDouble / totalGames) * 100).label(1).trim + "%)" )

          score
        }

        (key, scores.toArray)
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
      case (key, scores) =>

        val scoreSum = scores.sum.toDouble
        println(key.leftAlignedPadded(Specification.maxOptFactorLabelLength + 1).addColon + "average".addColon + scores.avg.toInt.label(6) + " - weight".addColon + (scoreSum / sumOfAllGames))

    }

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

  def play(context: Context, startGameCount:Int = 0): Array[Int] = {

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

  private def countDown(continue: Boolean, context: Context) = {
    if (continue && context.show) {

      print("\nstarting new game in ")
      Console.out.flush()

      // countdown timer
      (1 to 10).reverse.foreach { i =>
        print(i + "...")
        Console.out.flush()
        beep(context)
        Thread.sleep(500)
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

  def beep(context: Context): Unit = if (context.beep) toolKit.beep()

}
