/**
 * Created by nathan on 12/30/16.
 * parses command line and sets defaults
 */

import org.slf4j.LoggerFactory
import org.slf4j.MDC
import scala.language.reflectiveCalls


import ch.qos.logback.classic.{ Logger,  LoggerContext }

class Context(conf: Conf) {

  private val loggerContext: LoggerContext = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]

  val logger:Logger  = loggerContext.getLogger("root") // Logger("simulation_logger") // gets root logger - could also get it from loggerContext via loggerContext.getLogger("ROOT")

  sys.addShutdownHook(
    {
      logger.info("done")
      // val loggerContext: LoggerContext = LoggerFactory.getILoggerFactory().asInstanceOf[LoggerContext]
      loggerContext.stop()
      if (this.show) println("goodbye - i hoped you enjoyed this simulation") //}
    }
  )

  val jsonLogger:Logger = loggerContext.getLogger("json")

  // used to append a game number to a json file so we get a file per game
  def setJSONFileNameDiscriminators(gameNumber: Int, gameSeed: Int): Unit = {
    MDC.put("gameInfo", "game_" + gameNumber.toString + "_seed_" + gameSeed.toString)
  }

  // vars so you can change test specifications - consider other mechanisms if you wish
  val beep: Boolean = conf.beep()
  val fixedWeights:Boolean = conf.fixedWeights()

  var gamesToPlay: Int = conf.gamesToPlay()
  val generateWeightsGamesToPlay: Int = conf.weights.v1Games.getOrElse(0)
  val generatingWeights:Boolean = generateWeightsGamesToPlay > 0

  var stopGameAtRound: Int = conf.roundsToPlay()
  //noinspection VarCouldBeVal
  var parallel: Boolean = !conf.serial()
  var logJSON: Boolean = conf.logJSON()

  var show: Boolean = !conf.hide()
  //noinspection VarCouldBeVal
  var showRoundResultsOnly: Boolean = conf.showRoundResultsOnly()

  val stopAtNewHighScore: Boolean = !conf.continueAtNewHighScore()

  private val multiGameSeed = conf.sessionSeed()
  // either we will generate a new series of games from scratch, or take the provided multi game seed to redo a previously generated series
  private val randomizer = if (multiGameSeed == 0) new scala.util.Random() else new scala.util.Random(multiGameSeed)

  // get the gameSeed from the command line
  private var gameSeed = conf.gameSeed()

  // allows overriding the next game seed - used by weight generator
  def setGameSeed(seed: Int): Unit = gameSeed = seed

  // if there is no passed in gameSeed then generate it from the randomizer,
  // otherwise use the gameSeed that is either passed in via conf or set via setGameSeed
  // appended abs as there is no need for negative numbers to make the json file name confusing
  def getGameSeed: Int = (if (gameSeed == 0) randomizer.nextInt else gameSeed).abs

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

  def takeReplayPiecesForRound: Iterator[PieceLocCleared] = internalReplayListIterator.take(GamePieces.numPiecesInRound)

  var simulationSelfTest:Boolean = false

}

object Context {
  val FILE_HIGH_SCORE = ".highscore"
}
