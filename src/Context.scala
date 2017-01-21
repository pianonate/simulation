/**
 * Created by nathan on 12/30/16.
 * parses command line and sets defaults
 */

case class GameInfo(
  sessionHighScore: Int,
  machineHighScore: Int,
  gameCount:        Int,
  totalTime:        GameTimer
)

class Context(args: Array[String]) {

  // todo - just don't throw errors - shut things down in an orderly manner and output errors at the command line
  // todo - integrate a library that does command line processing as it's not core to this project

  import Context._
  private val validArguments = Set(
    continuousPlayName,
    maxSimulationsName,
    serialName,
    showName,
    stopGameAtRoundName

  )

  private val argMap: Map[String, String] = args.map(_.split(":"))
    .map({
      case a if a.length == 1 => (a(0), "true")
      case b if b.length == 2 => (b(0), b(1))
    })
    .toMap

  argMap.keys.foreach(key => { if (!validArguments(key)) throw new IllegalArgumentException(key) })

  // max simulations if you had 3 singletons chosen on an empty board:
  private val BOARD_UNOCCUPIED = Board.BOARD_SIZE * Board.BOARD_SIZE
  private val MAX_SIMULATION_ITERATIONS: Int = BOARD_UNOCCUPIED * (BOARD_UNOCCUPIED - 1) * (BOARD_UNOCCUPIED - 2)

  private def getIntArgValue(arg: String, default: Int): Int = {
    try {
      if (argMap.contains(arg)) argMap(arg).toInt else default
    } catch {
      case e: java.lang.NumberFormatException => throw new IllegalArgumentException("Expected an Int but got this: " + e.getMessage)
      case unknownException: Throwable        => throw unknownException
    }

  }

  private def getBooleanArgValue(arg: String, default: Boolean): Boolean = {
    if (argMap.contains(arg)) true else default
  }

  // vars so you can change test specifications - consider other mechanisms if you wish
  var continuousMode: Boolean = getBooleanArgValue(continuousPlayName, default = true)
  var maxSimulations: Int = getIntArgValue(maxSimulationsName, MAX_SIMULATION_ITERATIONS)
  //noinspection VarCouldBeVal
  var stopGameAtRound: Int = getIntArgValue(stopGameAtRoundName, 0)
  //noinspection VarCouldBeVal
  var serialMode: Boolean = getBooleanArgValue(serialName, default = false) // by default we are not in serial mode
  var show: Boolean = getBooleanArgValue(showName, default = true)

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

  val continuousPlayName = "-continuousPlay"
  val maxSimulationsName = "-maxSimulations"
  val stopGameAtRoundName = "-stopAtRound"
  val serialName = "-serial"
  val showName = "-show"
}
