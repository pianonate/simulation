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
    """.stripMargin)

  footer(
    """
      |
      |
      |coded by Nathan McCoy under Creative Commons Attribution-ShareAlike 4.0 International license
      |license text: https://creativecommons.org/licenses/by-sa/4.0/
      |code:https://github.com/rhialtotm/simulation
    """.stripMargin
  )

  val maxSimulations: ScallopOption[Int] = opt[Int](default = Some(MAX_SIMULATION_ITERATIONS), validate = (i) => i > 0 && i <= MAX_SIMULATION_ITERATIONS, descr = "integer limit for number of allowed simulations for each piece")

  val continueAtNewHighScore: ScallopOption[Boolean] = opt[Boolean](descr="continue playing if new high score is reached - default is to stop", name="continue")

  val logJSON: ScallopOption[Boolean] = opt[Boolean](descr="logs game json to json folder", name="logjson", short='j')

  val gamesToPlay: ScallopOption[Int] = opt[Int](default = Some(0), validate = (i) => i >= 0, name="games", descr = "number of games to play - 0 is continuous")
  val roundsToPlay: ScallopOption[Int] = opt[Int](default = Some(0), validate = (i) => i >= 0, name="rounds", descr = "end the game at this round number")

  val serial: ScallopOption[Boolean] = opt[Boolean](descr="play serial - if left blank, runs multithreaded - faster!")

  val hide: ScallopOption[Boolean] = opt[Boolean](descr="hides default output")

  val showRoundResultsOnly: ScallopOption[Boolean] = opt[Boolean](descr="use this to only display round results and not all results", short='o')

  val showWorst: ScallopOption[Boolean] = opt[Boolean](hidden=true, descr="show worst simulation choices each round - currently", short='t')

  val gameSeed: ScallopOption[Int] = opt[Int](default = Some(0), descr = "provide a seed to cause every game to play with the same pieces, if you don't specify then the pieces will be random")
  val gameSeedAllGames: ScallopOption[Int] = opt[Int](default = Some(0), descr = "provide a seed to play a series of games with the same seed value, replaying each game within the session with the same seed value - if you don't specify then the entire series will be random")

  val nobeep: ScallopOption[Boolean] = opt[Boolean](descr="silences end of game beeping")

  // the following are non-game arguments that will result in a no game being played
  val printPieces: ScallopOption[Boolean] = opt[Boolean](descr="print out the game pieces ")
  val weightGenerator: ScallopOption[Int] = opt[Int](default = Some(0), validate = (i) => i >= 0, descr = "default(0): generate the weights for each optimization factor based on how well they play - the integer value is how many games to play for each optFactor.  If the value is 0, it skips this")


  mainOptions = Seq(gamesToPlay, roundsToPlay, logJSON, hide, nobeep, serial)

  verify()

}