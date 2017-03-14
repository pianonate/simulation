import org.rogach.scallop.{ScallopOption, _}

/**
 * Created by nathan on 1/23/17.
 * cooler command line handling than what I wrote
 * prints nice --help output plus does error checking
 */

class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {

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
        |After placing pieces in a Simulation, the resultant board is scored for the following features:
        |""".stripMargin

    val end = """
                |
                |Each game generates random weights to be used for the whole game.  The integer scores
                |for the pieces are first normalized to a scale of 0 to 1, then multiplied by the weights.
                |The weighted sum of the scores determines the best Simulation.
                |
                |The best Simulation is then used to place pieces and continue onto the next round.""".stripMargin

    val explanations = Specification.getFeatureDescriptions

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

  val abridgedLogs: ScallopOption[Boolean] = opt[Boolean](descr = "when logging json, reduce the dataset")
  val beep: ScallopOption[Boolean] = opt[Boolean](descr = "turn on beeping at the end of each game")
  val continueAtNewHighScore: ScallopOption[Boolean] = opt[Boolean](descr = "continue playing if new high score is reached - default is to stop", name = "continue")
  val dimensions: ScallopOption[Int] = opt[Int](default = Some(Conf.DEFAULT_BOARD_SIZE), validate = (i) => i >= 5, descr = "board size wxh - default is 10")
  val fixedWeights: ScallopOption[Boolean] = opt[Boolean](short=Conf.fixedWeightArg, descr = "use weights hardcoded into the specification for the game. Default: Use random weights as currently, random plays better!")
  val gamesToPlay: ScallopOption[Int] = opt[Int](default = Some(0), validate = (i) => i >= 0, name = "games", descr = "number of games to play - 0 is continuous")
  val gameSeed: ScallopOption[Int] = opt[Int](default = Some(0), short = Conf.gameSeedArg, descr = "provide a seed to cause every game to play with the same pieces, if you don't specify then the pieces will be random")
  val hide: ScallopOption[Boolean] = opt[Boolean](descr = "hides default output")
  val logJSON: ScallopOption[Boolean] = opt[Boolean](descr = "logs game json to json folder", name = "logjson", short = 'j')
  val printPieces: ScallopOption[Boolean] = opt[Boolean](descr = "print out the game pieces ")
  val roundsToPlay: ScallopOption[Int] = opt[Int](default = Some(0), validate = (i) => i >= 0, name = "rounds", descr = "end the game at this round number")
  val serial: ScallopOption[Boolean] = opt[Boolean](descr = "play serial - if left blank, runs multi-threaded - faster!")
  val sessionSeed: ScallopOption[Int] = opt[Int](default = Some(0), short = Conf.sessionSeedArg, descr = "provide a seed to play a series of games with the same seed value, replaying each game within the session with the same seed value - if you don't specify then the entire series will be random (as long as you have not specified a --game-seed)")
  val showRoundResultsOnly: ScallopOption[Boolean] = opt[Boolean](descr = "use this to only display round results and not all results", short = 'o')

  mainOptions = Seq(gamesToPlay, roundsToPlay, logJSON, hide, beep, serial)

  verify()

}

object Conf {
  private val DEFAULT_BOARD_SIZE = 10
  val fixedWeightArg:Char = 'f'
  val gameSeedArg:Char = 'm'
  val sessionSeedArg:Char = 'e'
}