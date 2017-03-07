/**
 * Created by nathan on 12/20/16.
 * let Main be Main
 * GameRunner owns the responsibility of running Games and managing high score persistence
 */

import Implicits._
import java.awt.Toolkit
import java.io.PrintWriter

object GameRunner {

  case class WeightContext(v1:Boolean, context:Context) {

    context.gamesToPlay = 1

    private val v1Spec = Specification(filtered=false)

    def specification = {
      if (v1)
        v1Spec
      else {
        val doubleRandomizer = new scala.util.Random()

        // copy the current specification into a random specification
        val randomSpec = v1Spec.spec.map{ case(key,opt) =>
          (key, OptimizationFactor(opt.enabled, opt.key, opt.minimize, doubleRandomizer.nextDouble /* random! */, opt.minVal, opt.maxVal, opt.label, opt.explanation))
        }
        Specification(filtered=false,randomSpec)
      }
    }


    val longestKeyLength = v1Spec.spec.keys.map(_.length).max
    val iterations = context.generateWeightsGamesToPlay
    val totalGames = if (v1) iterations * v1Spec.length else iterations
    private val randomizer = new scala.util.Random(1)
    val seeds = Array.fill[Int](iterations)(randomizer.nextInt.abs)

  }

  private val allGamesTimer = new GameTimer

  private def  gamesPerMinute(games: Int) = math.floor(games / allGamesTimer.elapsedMinutes).toInt

  private def playOneGame(weightContext:WeightContext, gameIndex:Int, factorIndex:Int, optFactor:Option[OptimizationFactor]): Int = {

    val singleGameTimer = new GameTimer

    // each factor will play the same game to see how each performs against the same set of pieces
    weightContext.context.setGameSeed(weightContext.seeds(gameIndex))

    // play a game and get its score back - this seems a little obscure in terms of how to get the score...
    // could you be more clear?
    val score = play(weightContext.context, gameIndex)(0)

    val completed: Int = (factorIndex * weightContext.iterations) + (gameIndex + 1)

    val prefix = optFactor match {

      case Some(_) => optFactor.get.key.rightAlignedPadded(weightContext.longestKeyLength) +  " - " // v1
      case None => "" // v2

    }

    val result = prefix + "score: " + score.label(9).green +
      " - done in " + singleGameTimer.elapsedLabel.trim.rightAlignedPadded(11).green +
      " - game: " + completed.label(4).green + " out of " + weightContext.totalGames.toString.green +
      " (" + ((completed.toDouble / weightContext.totalGames) * 100).label(3,2).green + "%".green + ")" +
      " game seed: " + weightContext.context.getGameSeed.rightAligned(10).green +
      " elapsed: " + allGamesTimer.elapsedLabel.trim.green

    // output to screen and to main log file
    println(result)
    weightContext.context.logger.info(result)

    score

  }


  def generateWeightsV2(context:Context):Unit = {
    context.logger.info("generating weights with all weight randomized")
    context.logger.info("enabling json - 1 file per game")

    context.logJSON = true

    val weightContext = WeightContext(v1=false, context)

    (0 until weightContext.iterations) foreach {
      context.specification = weightContext.specification
      playOneGame(weightContext, _, 0, None)
    }
  }

  def generateWeightsV1(context: Context): Unit = {

    context.logger.info("generating weights by playing each optimization factor individually")

    val weightContext = WeightContext(v1=true, context)


    /*     
     todo - ask kevin
     i thought of an optimization to store previously generated weights in a file
     given that the randomization is always seeded with the same value (above)
     however because the game plays multi-threaded, it may be that a particular set
     of locs are chosen for a round that is different each time hou run it
     this is because different locations will result in the same weight value
     even if one is obviously better than another - they're not different when it comes to
     that one weight value
     do it doesn't seem legitimate to choose one result vs. another by simply
     favoring the run that you wrote to disk.
     check with Kevin or Brendan on this as maybe it doesn't make a difference 
     */

    val scores = weightContext.specification.spec.zipWithIndex.map {
      case ((key, optFactor), factorIndex) =>

        context.specification = Specification(optFactor)

        val gameScore: Seq[Int] = for (gameIndex <- 0 until weightContext.iterations) yield {

          playOneGame(weightContext, gameIndex, factorIndex, Some(optFactor))

        }

        (key, gameScore.toArray)
    }

    println

    val sumOfAllGames = scores.values.map(game => game.sum).sum

    println("factors".label + weightContext.specification.length.shortLabel)
    println("games per factor".label + weightContext.iterations.shortLabel)
    println("games played".label + weightContext.totalGames.shortLabel + " in " + allGamesTimer.elapsedLabel.trim)
    val endGamesPerMinute = gamesPerMinute(weightContext.totalGames)

    println("games/minute".label + endGamesPerMinute)
    println("set iterations to: " + (endGamesPerMinute * 10 / weightContext.specification.length) + " for 10 minutes of calculating")
    println("set iterations to: " + (endGamesPerMinute * 60 / weightContext.specification.length) + " for 1 hour of calculating")

    println
    scores.toSeq.sortBy(a => a._2.sum * -1).foreach {
      case (key, sortedScores) =>

        val scoreSum = sortedScores.sum.toDouble
        println(key.leftAlignedPadded(Specification.maxOptFactorKeyLength + 1).appendColon + "average".appendColon + sortedScores.avg.toInt.label(6) + " - weight".appendColon + (scoreSum / sumOfAllGames))

    }

    println

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

    def logGame(results: GameResults) {

      if (!context.generatingWeights) {

        val roundsPerSecond: Double = (results.rounds / results.gameTimer.elapsedSeconds).toDouble

        context.logger.info("game " + gameCount.label(4).green
          + " - score: " + results.score.label(7).green
          + " average: " + scores.avg.toInt.label(7).green
          + " high score: " + scores.max.label(7).green
          + " rounds: " + results.rounds.label(7).green
          + " rounds/s: " + roundsPerSecond.label(2, 2).green
          + " duration: " + results.gameTimer.elapsedLabel.green
          + " game seed: " + context.getGameSeed.rightAligned(10).green)
      }
    }

    var continue = true

    // run the game, my friend
    do {

      val machineHighScore = getHighScore
      val sessionHighScore = if (scores.isEmpty) 0 else scores.max
      val average = if (scores.isEmpty) 0 else scores.avg.toInt

      gameCount.inc()

      val gameInfo = MultiGameStats(average, sessionHighScore, machineHighScore, gameCount.value, allGamesTimer)

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
        "total elapsed time".label + allGamesTimer.elapsedLabel + "\n\n"

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
