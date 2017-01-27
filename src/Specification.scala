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

import Implicits._

case class Specification(spec: Array[OptimizationFactor]) {

  val length: Int = spec.length

  def apply(i: Int): OptimizationFactor = spec(i)

  def getOptimizationFactorExplanations: String = {
    // used by showGameStart
    spec.map(optFactor => "* " + optFactor.label + " - " + optFactor.explanation).mkString("\n")
  }

  private[this] def greenifyResult(optLabel: Option[String], isGreen: Boolean, value: Int): String = {
    // maximized results are negated to work with Simulation.compare so
    // as not to have to jump through hoops in that method
    // use math.abs to show them to the user in a way that makes sense

    val result = value.abs
    val resultString = optLabel match {
        case s: Some[String] => s.get.optFactorLabel + (if (isGreen) result.greenLabel else result.redLabel)
        case _               => (if (isGreen) result.greenLabel else result.redLabel).parens
      }

    resultString
  }


  def getImprovedResultsString(simulationResults: List[SimulationInfo], chosen: Simulation, showWorst:Boolean): String = {
    // the improvement comes from gathering all simulation results and then color coding for the best
    // result out of all permutations rather than just comparing best and worst on a row by row basis
    // this is FAR superior

    // for outputting the results this is the best individual result at any position
    // concatenate the results from the best and the worst, transpose it for finding min, then turn that into an array
    // Boom! - best choices for each result
    val bestOfAll = (simulationResults.map(_.best.results) ++ simulationResults.map(_.worst.results)).transpose.map(_.min).toArray

    def getResultString(simulationResult: SimulationInfo, simulationIndex: Int): String = {

      def handleOptFactor(optFactor: OptimizationFactor, bestVal: Int, worstVal: Int, topValIndex: Int, showWorst:Boolean): String = {

        val topVal = bestOfAll(topValIndex)

        greenifyResult(Some(optFactor.label), bestVal == topVal, bestVal) +
          ( if (showWorst) greenifyResult(None, worstVal == topVal, worstVal) else "" )
      }

      val best = simulationResult.best.results

      val worst = if (showWorst)
        simulationResult.worst.results
      else
        simulationResult.worst.emptyResults // no need to calculate it we're not showing

      (simulationIndex + 1) + ": " + simulationResult.pieces.label + " -" +
        spec
        .zip(best).zip(worst).zipWithIndex
        .map(tup => (tup._1._1._1, tup._1._1._2, tup._1._2, tup._2))
        .map(tup => handleOptFactor(tup._1, tup._2, tup._3, tup._4, showWorst))
        .mkString(" -")

    }

    simulationResults.zipWithIndex.map { tup =>

      val result = tup._1
      val index = tup._2

      val s = getResultString(result, index) + " - simulations: " + (result.simulatedCount + result.unsimulatedCount).label(9) + result.elapsedMs.msLabel(5)

      //todo - fix: there seems to be a situation where it underlines two rows that have the same results

      // underline is for closers (Glengarry Glen Ross)
      if (result.best == chosen) {
        val parts = s.splitAt(s.indexOf(" ") + 1)
        parts._1 + StringFormats.UNDERLINE + parts._2.split(StringFormats.ESCAPE).mkString(StringFormats.UNDERLINE + StringFormats.ESCAPE) + StringFormats.SANE
      } else
        StringFormats.SANE + s
    }.mkString("\n")
  }

  def getBoardResultString(boardResult: Array[Int]): String = {

    // this is called from a board placement result during the actual placing of pieces post-simulation
    // we keep board placement results separate on the one board that the whole game runs on
    // so that we can compare expected results from a simulation with actual results on the board
    // additionally, this mechanism allows us to display line clearing.
    spec
      .zip(boardResult)
      .map(tup => greenifyResult(Some(tup._1.label), isGreen = true, tup._2) )
      .mkString(" -")
  }

}

case class OptimizationFactor(
  enabled:     Boolean,
  fieldName:   String,
  minimize:    Boolean,
  label:       String,
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
  val maxContiguousName = "openContiguous"
  val openLinesName = "openLines"

  private val MINIMUM_SPEC_LENGTH: Int = 5

  val fullSpecification = Array(

    // specification provides the ordering of the optimization as well as whether a particular optimization is maximized or minimized
    // you'll need to update board.results, Simulation.compare and MINIMUM_SPEC_LENGTH if you change the length of the fullSpecification Array
    // other than that, you can rearrange rows in the specification, or turn entries off or on at will
    // much more flexible than it used to be

    // todo - run through all specification combinations of off and on and run 1000? games on each to see which specification is the best
    //        after a thousand games
    OptimizationFactor(enabled = true, maximizerCountName, maximize, "maximizer", "positions in which a 3x3 piece can fit"),
    OptimizationFactor(enabled = true, occupiedCountName, minimize, "occupied", "occupied positions"),
    OptimizationFactor(enabled = true, fourNeighborsName, minimize, "4 neighbors", "number of positions surrounded on all 4 sides"),
    OptimizationFactor(enabled = true, threeNeighborsName, minimize, "3 neighbors", "number of positions surrounded on 3 of 4 sides"),
    OptimizationFactor(enabled = true, maxContiguousName, maximize, "contiguous open lines", "number of lines (either horizontal or vertical) that are open and contiguous"),
    OptimizationFactor(enabled = false, openLinesName, maximize, "open Rows & Cols", "count of open rows plus open columns"),
    OptimizationFactor(enabled = true, twoNeighborsName, minimize, "2 neighbors", "number of positions surrounded on 2 of 4 sides")


  )

  // by default return the full specification
  def apply(): Specification = {
    val filteredSpec = Specification(fullSpecification.filter(_.enabled))
    require(filteredSpec.length >= MINIMUM_SPEC_LENGTH)
    filteredSpec
  }

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