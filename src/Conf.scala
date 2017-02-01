import org.rogach.scallop._

/**
 * Created by nathan on 1/23/17.
 * cooler command line handling than what I wrote
 * prints nice --help output plus does error checking
 */

class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
  // max simulations if you had 3 singletons chosen on an empty board:
  private val BOARD_UNOCCUPIED = Board.BOARD_SIZE * Board.BOARD_SIZE
  private val MAX_SIMULATION_ITERATIONS: Int = BOARD_UNOCCUPIED * (BOARD_UNOCCUPIED - 1) * (BOARD_UNOCCUPIED - 2)
  private val ERASE_TERMINAL_BUFFER_EVERY_N_ROUNDS = 1000

  version("simulation 0.1 ")
  banner(
    """
      |this is only a simulation of the blocks game, watch it go!
      |you can use or omit any of the following arguments as they all have proper defaults
      |
    """.stripMargin
  )

  val beep: ScallopOption[Boolean] = toggle(default = Some(true), descrYes = "default: beep when game finishes", descrNo = "don't beep at game end")
  val continuousPlay: ScallopOption[Boolean] = toggle(default = Some(true), descrYes = "default: simulation will run continuously", descrNo = "simulation will play through once till a game end occurs")
  val maxSimulations: ScallopOption[Int] = opt[Int](default = Some(MAX_SIMULATION_ITERATIONS), validate = (i) => i > 0 && i <= MAX_SIMULATION_ITERATIONS, descr = "integer limit for number of allowed simulations for each piece")
  val endGameAtRound: ScallopOption[Int] = opt[Int](default = Some(0), validate = (i) => i >= 0, descr = "end the game at this round number")
  val parallel: ScallopOption[Boolean] = toggle(default = Some(true), descrYes = "default: uses multiple threads - faster!", descrNo = "simulation will run single threaded")
  val show: ScallopOption[Boolean] = toggle(default = Some(true), descrYes = "default: show the game as it is playing", descrNo = "hide the game")
  val randomSeed: ScallopOption[Int] = opt[Int](default = Some(0), descr = "provide a seed to cause game to play with same pieces")
  val showWorst: ScallopOption[Boolean] = toggle(default = Some(false), descrYes = "show worst simulation choices each round", descrNo = "default:hide worst simulation choices each round - faster")
  val stopAtNewHighScore: ScallopOption[Boolean] = toggle(default = Some(true), short='n', descrYes = "default:stop a continuous play game if a new high score is reached", descrNo = "don't stop a continuous play game if a new high score is reached")

  val eraseTerminalBufferAt: ScallopOption[Int] = opt[Int](
    default = Some(ERASE_TERMINAL_BUFFER_EVERY_N_ROUNDS),
    validate = (i) => i > 0 && i <= ERASE_TERMINAL_BUFFER_EVERY_N_ROUNDS,
    descr = "default:" + ERASE_TERMINAL_BUFFER_EVERY_N_ROUNDS + " rounds.  " +
      "Once terminal buffer lines are exceeded on mac os x it will eat lines at end of game.  " +
      "use this value to specify the number of rounds after which it will clear the terminal buffer. " +
      "this is a stupid workaround to an issue on mac os x terminal.  if you use all available memory, " +
      "then terminal becomes unresponsive.  if you set it to use n lines (mine is 100,000), then you get the bug that this parameter is designed to avoid."
  )

  footer(
    """
      |coded by Nathan McCoy under Creative Commons Attribution-ShareAlike 4.0 International license
      |license text: https://creativecommons.org/licenses/by-sa/4.0/
      |code:https://github.com/rhialtotm/simulation
    """.stripMargin
  )

  verify()
}
