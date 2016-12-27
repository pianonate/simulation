/**
 * Created by nathan on 12/20/16.
 * let Main be Main
 * GameRunner owns the responsibility of running Games and managing high score persistence
 */
import GameUtil._
import java.io.PrintWriter

//todo:  right align scores correctly
object GameRunner {

  val HIGH_SCORE_FILE = ".highscore"

  def play(): Unit = {

    // after open lines optimization 22,603
    // playing it many times, sped up with some luck - 26,914

    // different than game continuous mode which simply
    // controls whether you hit enter to place the next piece
    // Main continuous mode means continuous play - you have to ctrl-c out of it
    val CONTINUOUS_MODE = true

    import scala.collection.mutable.ListBuffer

    val scores = new ListBuffer[Int]
    val rounds = new ListBuffer[Int]
    val simulationsPerSecond = new ListBuffer[Int]

    Game.showGameStart()

    // run the game, my friend
    do {

      val machineHighScore = getHighScore

      val game = new Game(if (scores.isEmpty) 0 else scores.max)
      val results = game.run(machineHighScore)

      scores.append(results._1)
      rounds.append(results._2)
      simulationsPerSecond.append(results._3)

      val sessionHighScore = scores.max
      val allTimeHighScore = List(machineHighScore, sessionHighScore).max
      val mostRounds = rounds.max
      val bestPerSecond = simulationsPerSecond.max

      if (sessionHighScore > machineHighScore)
        saveHighScore(sessionHighScore)

      println
      println
      println("MULTIPLE GAME STATS")
      println
      println(labelFormat.format("Games Played") + numberFormat.format(scores.size))
      println(labelFormat.format("High Score") + getScoreString(numberFormat, sessionHighScore))
      println(labelFormat.format("All Time High Score") + getScoreString(numberFormat, allTimeHighScore))
      println(labelFormat.format("Most Rounds") + numberFormat.format(mostRounds))
      println(labelFormat.format("Most Simulations/Second") + numberFormat.format(bestPerSecond))

      if (CONTINUOUS_MODE) {
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

  def saveHighScore(highScore: Int): Unit = {
    val pw = new PrintWriter(HIGH_SCORE_FILE)
    pw.write(highScore.toString)
    pw.close()

  }

  def getHighScore: Int = {

    import scala.io.Source

    try {
      return Source.fromFile(HIGH_SCORE_FILE).getLines.mkString.toInt

    } catch {
      case _: Throwable =>
        val pw = new PrintWriter(HIGH_SCORE_FILE)
        pw.write("0")
        pw.close()
    }

    0

  }

}
