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

  // todo - find out more about what to put here
  version("simulation 0.1 - creative commons license - nathan mccoy")
  banner(
    """
      |this is only a simulation of the blocks game, watch it go!
      |you can use or ommit any of the following arguments as they all have proper defaults
      |
      |
    """.stripMargin)
  val continuousPlay = toggle(default=Some(true), descrYes="default: simulation will run continuously", descrNo="simulation will play through once till a game end occurs")
  val maxSimulations = opt[Int](default = Some(MAX_SIMULATION_ITERATIONS), validate = ((i) => i>0 && i<=MAX_SIMULATION_ITERATIONS) , descr="integer limit for number of allowed simulations for each piece")
  val endGameAtRound = opt[Int](default = Some(0), validate=((i) => i>=0), descr="end the game at this round number")
  val parallel = toggle(default = Some(true), descrYes="default: uses multiple threads - faster!",  descrNo="simulation will run single threaded")
  val show = toggle(default = Some(true), descrYes="default: show the game as it is playing", descrNo="hide the game")
  val randomSeed = opt[Int](default = Some(0), descr="provide a seed to cause game to play with same pieces")
  verify()
}
