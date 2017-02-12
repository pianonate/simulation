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


  private val header = {

    val begin =
      """SIMULATION GAME INFO
        |
        |This game works by first selecting 3 pieces from the set of all possible pieces.
        |Then it will try all possible orderings (permutations) of placements of those 3 pieces
        |(up to six permutations will be selected depending on whether there are any duplicates).
        |If no lines are cleared then only the first permutation will be evaluated as piece order
        |won't matter so no reason in simulating any other orderings.
        |
        |Then for each permutation, it will try each of the 3 pieces in order on all possible
        |locations on the board, checking to see whether any lines are cleared between each piece placement.
        |Each placement of three pieces along with the board state after placing the third piece is
        |called a Simulation.
        |
        |Each of the following optimization factors will be evaluated on the final board state:""".stripMargin

    val end = """
                |
                |Taking all of these factors into account, based on their weightings, the best simulation will be chosen.
                |
                |The best simulation is then used to place pieces and continue onto the next round.""".stripMargin

    val explanations = Specification().getOptimizationFactorExplanations

    begin + "\n" + explanations + end
  }

  version("simulation 0.1 ")

  banner(header +
    """
      |
      |You can use or omit any of the following arguments as they all have proper defaults
      |
    """.stripMargin
  )

  footer(
    """
      |
      |
      |coded by Nathan McCoy under Creative Commons Attribution-ShareAlike 4.0 International license
      |license text: https://creativecommons.org/licenses/by-sa/4.0/
      |code:https://github.com/rhialtotm/simulation
    """.stripMargin
  )

  val beep: ScallopOption[Boolean] = toggle(default = Some(true), descrYes = "default: beep when game finishes", descrNo = "don't beep at game end")
  val maxSimulations: ScallopOption[Int] = opt[Int](default = Some(MAX_SIMULATION_ITERATIONS), validate = (i) => i > 0 && i <= MAX_SIMULATION_ITERATIONS, descr = "integer limit for number of allowed simulations for each piece")

  val endGameAtRound: ScallopOption[Int] = opt[Int](default = Some(0), validate = (i) => i >= 0, descr = "end the game at this round number")
  val stopAtNewHighScore: ScallopOption[Boolean] = toggle(default = Some(true), short = 'n', descrYes = "default:stop a continuous play game if a new high score is reached", descrNo = "don't stop a continuous play game if a new high score is reached")


  val gamesToPlay: ScallopOption[Int] = opt[Int](default = Some(0), validate = (i) => i >= 0, descr = "anything greater than 0 will invoke for this number of games.  0 means play continuously")

  val parallel: ScallopOption[Boolean] = toggle(default = Some(true), descrYes = "default: uses multiple threads - faster!", descrNo = "simulation will run single threaded")

  val show: ScallopOption[Boolean] = toggle(default = Some(true), descrYes = "default: show the game as it is playing", descrNo = "hide the game")
  val showRoundResultsOnly: ScallopOption[Boolean] = toggle(default = Some(false), descrYes = "use this to only display round results", descrNo = "default: use this to show all results")
  val showWorst: ScallopOption[Boolean] = toggle(default = Some(false), descrYes = "show worst simulation choices each round", descrNo = "default:hide worst simulation choices each round - faster")

  val randomSeed: ScallopOption[Int] = opt[Int](default = Some(0), descr = "provide a seed to cause game to play with same pieces")


  // the following are non-game arguments that will result in a no game being played
  val printPieces: ScallopOption[Boolean] = toggle(default = Some(false), descrYes="print out the game pieces", descrNo="default: don't print out the game pieces")
  val weightGenerator: ScallopOption[Int] = opt[Int](default = Some(0),validate = (i) => i >= 0, descr = "default(0): generate the weights for each optimization factor based on how well they play - the integer value is how many games to play for each optFactor.  If the value is 0, it skips this")

  verify()



}