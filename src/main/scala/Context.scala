/**
 * Created by nathan on 12/30/16.
 * parses command line and sets defaults
 */

import scala.util.Random

import org.slf4j.LoggerFactory

import com.typesafe.scalalogging.Logger

import ch.qos.logback.classic.LoggerContext

class Context(conf: Conf) {

  private val loggerContext: LoggerContext = LoggerFactory.getILoggerFactory().asInstanceOf[LoggerContext]

  val logger = Logger("simulation_logger") // gets root logger - could also get it from loggerContext via loggerContext.getLogger("ROOT")
  logger.info("starting simulation")

  val jsonLogger = loggerContext.getLogger("json")

  sys.addShutdownHook(
    {
      logger.info("stopping simulation")
      // val loggerContext: LoggerContext = LoggerFactory.getILoggerFactory().asInstanceOf[LoggerContext]
      loggerContext.stop()
      if (this.show) println("goodbye - i hoped you enjoyed this simulation") //}
    }
  )

  // vars so you can change test specifications - consider other mechanisms if you wish
  val beep: Boolean = conf.beep()
  var gamesToPlay: Int = conf.gamesToPlay()
  val generateWeightsGamesToPlay: Int = conf.weightGenerator()
  //noinspection VarCouldBeVal
  var maxSimulations: Int = conf.maxSimulations()
  var stopGameAtRound: Int = conf.endGameAtRound()
  var parallel: Boolean = conf.parallel()
  val logJSON: Boolean = conf.logJSON()
  var show: Boolean = conf.show()
  var showRoundResultsOnly: Boolean = conf.showRoundResultsOnly()

  //noinspection VarCouldBeVal
  var showWorst: Boolean = conf.showWorst()

  val stopAtNewHighScore: Boolean = conf.stopAtNewHighScore()

  // TODO - always set a random seed and then log it with the game score
  //        so you can replay that game with a new algo if you wish
  //        the thing to do is to use conf.randomSeed() or generate one
  //        as a def to create a set of new seeds for each game that's played
  //
  var randomSeed: Int = conf.randomSeed()
  /* def gameSeed:Int = {
    val randomizer: Random = if (seed > 0) new scala.util.Random(seed) else new scala.util.Random()

  }*/

  //noinspection VarCouldBeVal
  var specification: Specification = Specification()

  var replayGame: Boolean = false
  //noinspection VarCouldBeVal
  var ignoreSimulation: Boolean = true

  private var internalReplayListIterator = List[PieceLocCleared]().toIterator

  def setReplayList(plcList: List[PieceLocCleared]): Unit = {
    replayGame = true
    internalReplayListIterator = plcList.toIterator
  }
  def takeReplayPiecesForRound: Iterator[PieceLocCleared] = internalReplayListIterator.take(3)

}

object Context {
  val FILE_HIGH_SCORE = ".highscore"
  val FILE_SAVED_GAME = ".simulationSavedGame"
}
