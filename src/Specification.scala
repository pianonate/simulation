/**
 * Created by nathan mccoy on 1/15/17.
 * introducing the Specification in order to configure it on the context
 * so that tests can be run to ensure that all specification combinations
 *   (from 1 to Specification.length) and then the permutations of those
 *   combinations all result in valid games that can play
 *   This is a general test that is useful
 *
 *   Also, this paves the way to run the game by trying all of the above
 *   n times to see which combination/permutation of specifications
 *   results in the highest scores...
 */
case class Specification(spec: Array[OptimizationFactor]) {
  val length: Int = spec.length
  def apply(i: Int): OptimizationFactor = spec(i)
  def getOptimizationFactorExplanations: String = {
    // used by showGameStart
    spec.map(optFactor => "* " + optFactor.resultLabel + " - " + optFactor.explanation).mkString("\n")
  }

  private[this] val resultFormat = "%2d"
  private[this] val resultParenFormat = " (" + resultFormat + ")"
  private[this] val resultLabelFormat = " %s: "

  private[this] def greenifyResult(isGreen: Boolean, value: Int, valFormat: String, label: String, labelFormat: String): String = {
    // maximized results are negated to work with Simulation.compare so
    // as not to have to jump through hoops in that method
    // use math.abs to show them to the user in a way that makes sense
    val result = valFormat.format(math.abs(value))
    labelFormat.format(label) + (if (isGreen) Game.GREEN + result else Game.RED + result) + Game.SANE
  }

  def getImprovedResultsString(simulationResults: List[SimulationInfo], chosen: Simulation): String = {
    // the improvement comes from gathering all simulation results and then color coding for the best
    // result out of all permutations rather than just comparing best and worst on a row by row basis
    // this is FAR superior

    // for outputting the results this is the best individual result at any position
    // concatenate the results from the best and the worst, transpose it for finding min, then turn that into an array
    // Boom! - best choices for each result
    val bestOfAll = (simulationResults.map(_.best.results) ++ simulationResults.map(_.worst.results)).transpose.map(_.min).toArray

    def getResultString(simulationResult: SimulationInfo, simulationIndex: Int): String = {

      def handleOptFactor(optFactor: OptimizationFactor, bestVal: Int, worstVal: Int, topValIndex: Int): String = {
        val topVal = bestOfAll(topValIndex)
        greenifyResult(bestVal == topVal, bestVal, resultFormat, optFactor.resultLabel, resultLabelFormat) + greenifyResult(worstVal == topVal, worstVal, resultParenFormat, "", "")
      }

      val best = simulationResult.best.results

      val worst = simulationResult.worst.results

      (simulationIndex + 1) + ": " + simulationResult.pieces.map(_.name).mkString(", ") + " -" +
        spec
        .zip(best).zip(worst).zipWithIndex
        .map(tup => (tup._1._1._1, tup._1._1._2, tup._1._2, tup._2))
        .map(tup => handleOptFactor(tup._1, tup._2, tup._3, tup._4))
        .mkString

    }

    def getPerformanceString(result: SimulationInfo): String = {
      val largeValuesFormat = "%,5d"

      val simulationCountString = largeValuesFormat.format(result.simulationCount)

      val durationString = largeValuesFormat.format(result.elapsed) + "ms"

      " - simulations: " + simulationCountString + " in " + durationString
    }

    simulationResults.zipWithIndex.map { tup =>
      val r = tup._1
      val index = tup._2

      val s = getResultString(r, index) + getPerformanceString(r)

      // underline is for closers (Glengarry Glen Ross)
      if (r.best == chosen) {
        val parts = s.splitAt(s.indexOf(" ") + 1)
        parts._1 + Game.UNDERLINE + parts._2.split(Game.ESCAPE).mkString(Game.UNDERLINE + Game.ESCAPE) + Game.SANE
      } else
        Game.SANE + s
    }.mkString("\n")
  }

  def getBoardResultString(boardResult: Array[Int]): String = {
    // this is called from a board placement result during the actual placing of pieces post-simulation
    // we keep board placement results separate on the one board that the whole game runs on
    // so that we can compare expected results from a simulation with actual results on the board
    // additionally, this mechanism allows us to display line clearing.
    // all of the above is to say, this results string is fairly complex because of keeping these
    // things separate.  for now, this is acceptable
    // at least the results are guided by the specification.  Previous instances were not and
    // there was a lot of duplication and gnashing of teeth

    val longLabelFormat = " -" + resultLabelFormat

    var first = true

    def handleOptFactor(optFactor: OptimizationFactor, resultVal: Int): String = {
      val theLabel = if (first) { first = false; resultLabelFormat } else longLabelFormat
      val green = true
      greenifyResult(green, resultVal, resultFormat, optFactor.resultLabel, theLabel)
    }

    spec
      .zip(boardResult)
      .map(tup => handleOptFactor(tup._1, tup._2))
      .mkString

  }

}

case class OptimizationFactor(
  enabled:     Boolean,
  fieldName:   String,
  minimize:    Boolean,
  resultLabel: String,
  explanation: String
)

object Specification {

  // for readability
  private val minimize = true
  private val maximize = false

  val occupiedCountName = "occupiedCount"
  val maximizerCountName = "maximizerCount"
  val fourNeighborsName = "fourNeighbors"
  val threeNeighborsName = "threeNeighbors"
  val twoNeighborsName = "twoNeighbors"
  val islandMaxName = "islandMax"
  val maxContiguousName = "openContiguous"
  val openLinesName = "openLines"

  val fullSpecification = Array(

    // specification provides the ordering of the optimization as well as whether a particular optimization is maximized or minimized
    // you'll need to update board.results and Simulation.compare if you change the length of the fullSpecification Array
    // other than that, you can rearrange rows in the specification, or turn entries off or on at will
    // much more flexible than it used to be

    // todo - run through all specification combinations of off and on and run 1000? games on each to see which specification is the best
    //        after a thousand games
    OptimizationFactor(enabled = true, maximizerCountName, maximize, "maximizer", "positions in which a 3x3 piece can fit"),
    OptimizationFactor(enabled = true, occupiedCountName, minimize, "occupied", "occupied positions"),
    OptimizationFactor(enabled = true, fourNeighborsName, minimize, "4 neighbors", "number of positions surrounded on all 4 sides"),
    OptimizationFactor(enabled = true, threeNeighborsName, minimize, "3 neighbors", "number of positions surrounded on 3 of 4 sides"),
    OptimizationFactor(enabled = true, maxContiguousName, maximize, "contiguous open lines", "number of lines (either horizontal or vertical) that are open and contiguous"),
    OptimizationFactor(enabled = true, twoNeighborsName, minimize, "2 neighbors", "number of positions surrounded on 2 of 4 sides"),
    OptimizationFactor(enabled = false, openLinesName, maximize, "open Rows & Cols", "count of open rows plus open columns"),
    OptimizationFactor(enabled = false, islandMaxName, maximize, "islandMax", "largest number of connected, unoccupied positions")

  )

  // by default return the full specification
  def apply(): Specification = Specification(fullSpecification.filter(_.enabled))

  /**
   * used for testing purposes (for now) and eventually for an exhaustive run
   * through of all possible permutations and combinations of specifications
   * @return
   */
  def getAllSpecifications: Array[Array[Array[OptimizationFactor]]] = {

    val r = (1 to fullSpecification.length).toArray

    val combinations = for { i <- r } yield fullSpecification.combinations(i).toArray

    val result = for {
      comboArray <- combinations
      combo <- comboArray
    } yield combo.permutations.toArray

    result

  }

  // one of the optimizations is to ensure that the maximum number of
  // maximum pieces can fit on a board from all the boards simulated in the permutation of a set of pieces
  // apparently it's important that this be declared after Game.CYAN is declared above :)
  // this is not private because we show the maximizer piece at game start
  val maximizer: Box = Pieces.bigBox
}