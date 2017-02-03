/**
 * Created by nathan mccoy on 1/15/2017.
 *
 * introducing the Specification in order to configure it on the context
 *
 *  so that tests can be run to ensure that all specification combinations
 *   (from 1 to Specification.length) and then the permutations of those
 *   combinations all result in valid games that can play
 *   This is a general test that is useful
 *
 *   Also, this paves the way to run the game by trying all of the above
 *   n times to see which combination/permutation of specifications
 *   results in the highest scores...
 */

import Implicits._
import scala.collection.immutable.ListMap

case class Specification(spec: ListMap[String, OptimizationFactor]) {

  val length: Int = spec.size

  //def apply(i: Int): OptimizationFactor = weightedSpec(i)
  def apply(s: String): OptimizationFactor = weightedSpec(s)

  // todo - show and count the times when a best is chosen
  //        where a lower precedence optimization factor is chosen over a higher precedence
  //        because the lower precedence factor is better enough to make it the winner
  //        the sort chose - from the best of the best...
  private val weightedSpec: ListMap[String, OptimizationFactor] = {

    // divide each by 100 and then the next one divies previous by 100
    // val weights = Array.tabulate(length)(n => 1 / (math.pow(10, n) * math.pow(10, n)))
    val weights = spec.map(each => each._2.initialWeightFactor).scanLeft(1.0)((a, b) => a / b).tail

    val totalOfWeights = weights.sum
    val normalizedWeights = weights.map(_ / totalOfWeights)

    // divide weights by totalOfWeights so that when you multiply weights time values, we will get a distribution from 0 to 1
    // beautiful
    // Brendan's suggestion
    val weighted = spec.zip(normalizedWeights).map {
      case (specEntry, weight) =>
        specEntry match {
          case (key, opt) => (key, OptimizationFactor(opt.enabled, opt.fieldName, opt.minimize, weight, opt.initialWeightFactor, opt.maxVal, opt.label, opt.explanation))
        }
    }

    weighted
  }

  // provided so that the getNamedResult function in boardScore can iterate over a while loop
  // as using named keys into the ListMap along with apply() access is VERY SLOW
  val optimizationFactors: Array[OptimizationFactor] = weightedSpec.values.toArray

  private def getOptimizationFactor(name: String): OptimizationFactor = {
    // a filtered spec is passed in - non non-enabled specifications are allowed
    // because of this, the direct values we provide on this Specification instance
    // need to provide a default for non-enabled values
    if (weightedSpec.contains(name))
      weightedSpec(name)
    else
      OptimizationFactor(false, name, false, 0.0, 0.0, 0, "", "")
  }

  val occupiedOptFactor: OptimizationFactor = getOptimizationFactor(Specification.occupiedCountName)
  val maximizerOptFactor: OptimizationFactor = getOptimizationFactor(Specification.maximizerCountName)
  val fourNeighborsOptFactor: OptimizationFactor = getOptimizationFactor(Specification.fourNeighborsName)
  val threeNeighborOptFactor: OptimizationFactor = getOptimizationFactor(Specification.threeNeighborsName)
  val twoNeighborsOptFactor: OptimizationFactor = getOptimizationFactor(Specification.twoNeighborsName)
  val maxContiguousLinesOptFactor: OptimizationFactor = getOptimizationFactor(Specification.maxContiguousName)
  val openLinesOptFactor: OptimizationFactor = getOptimizationFactor(Specification.openLinesName)

  def getOptimizationFactorExplanations: String = {
    // used by showGameStart
    spec.map(optFactor => "* " + optFactor._2.label + " - " + optFactor._2.explanation).mkString("\n")
  }

  private val longestOptimizationFactorName = weightedSpec.values.map(_.fieldName.length).max

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

  def getImprovedSimulationResultsString(simulationResults: List[SimulationInfo], chosen: Simulation, showWorst: Boolean): String = {

    val piecePrefixLength = 0.indexLabel.length + longestOptimizationFactorName

    val piecesString = simulationResults
      .zipWithIndex
      .map { case (result, index) => result.pieces.permutationShowPiecesHeader(index) }
      .spreadHorizontal(piecePrefixLength, StringFormats.DOUBLE_VERTICAL_LINE + " ")


    // get the scores from the simulationInfo
    //val scores: List[List[ScoreComponent]] = simulationResults.map(info => info.best.board.score.scores)


/*    val specString = weightedSpec
      .values
      .zipWithIndex
      .map { case (opt, index) => index.indexLabel + opt.fieldName +
        simulationResults.map(info => info(index).toString)}
      .mkString("\n")*/

    piecesString + "\n" /*+ specString*/
  }

  def getSimulationResultsString(simulationResults: List[SimulationInfo], chosen: Simulation, showWorst: Boolean): String = {
    // the improvement comes from gathering all simulation results and then color coding for the best
    // result out of all permutations rather than just comparing best and worst on a row by row basis
    // this is FAR superior

    // for outputting the results this is the best individual result at any position
    // concatenate the results from the best and the worst, transpose it for finding min, then turn that into an array
    // Boom! - best choices for each result
    val bestOfAll = (
      simulationResults.map(_.best.results) ++ // best result
      (if (showWorst) simulationResults.map(_.worst.get.results) else List[Array[Int]]()) // worst results (or empty if not showing anything)
    ).transpose.map(_.min).toArray // sort out the best of all...

    def getResultString(simulationResult: SimulationInfo, simulationIndex: Int): String = {

      def handleOptFactor(optFactor: OptimizationFactor, bestVal: Int, worstVal: Int, topValIndex: Int, showWorst: Boolean): String = {

        val topVal = bestOfAll(topValIndex)

        greenifyResult(Some(optFactor.label), bestVal == topVal, bestVal) +
          (if (showWorst) greenifyResult(None, worstVal == topVal, worstVal) else "")
      }

      val best = simulationResult.best.results

      val worst = if (showWorst)
        simulationResult.worst.get.results
      else
        // todo - can we get empty results from the context rather than
        // storing it redundantly on each simulation?
        simulationResult.best.emptyResults

      (simulationIndex + 1) + ": " + simulationResult.pieces.label + " -" +
        spec
        .zip(best).zip(worst).zipWithIndex
        .map(tup => (tup._1._1._1, tup._1._1._2, tup._1._2, tup._2))
        .map(tup => handleOptFactor(tup._1._2, tup._2, tup._3, tup._4, showWorst))
        .mkString(" -")

    }

    simulationResults.zipWithIndex.map { tup =>

      val result = tup._1
      val index = tup._2

      val s = "weight: " + result.best.weightedSum.weightLabel + " - " + getResultString(result, index) + " - simulations: " + (result.simulatedCount + result.unsimulatedCount).label(9) + result.elapsedMs.msLabel(5)

      // underline is for closers (Glengarry Glen Ross)
      if (result.best == chosen) {
        val parts = s.splitAt(s.indexOf(" ") + 1) // start underlining after the #:_ at the beginnning of each line
        parts._1 +
          StringFormats.UNDERLINE + // thus begins the underlining
          parts._2 // it seems to make it work in both terminal and in intellij IDE, have to do both sets of splits below
          .split(" ").mkString(StringFormats.UNDERLINE + " ") // starting at all spaces
          .split(StringFormats.ESCAPE) // split on escape characters // underlines starting at all color codings
          .mkString(StringFormats.UNDERLINE + StringFormats.ESCAPE) + StringFormats.SANE
      } else
        s
    }.mkString("\n")
  }

  def getBoardResultString(boardResult: Array[Int]): String = {

    // this is called from a board placement result during the actual placing of pieces post-simulation
    // we keep board placement results separate on the one board that the whole game runs on
    // so that we can compare expected results from a simulation with actual results on the board
    // additionally, this mechanism allows us to display line clearing.
    spec
      .zip(boardResult)
      .map(tup => greenifyResult(Some(tup._1._2.label), isGreen = true, tup._2))
      .mkString(" -")
  }

}

case class OptimizationFactor(
  enabled:             Boolean,
  fieldName:           String,
  minimize:            Boolean,
  weight:              Double,
  initialWeightFactor: Double,
  maxVal:              Int,
  label:               String,
  explanation:         String
)

object Specification {

  // one of the optimizations is to ensure that the maximum number of
  // maximum pieces can fit on a board from all the boards simulated in the permutation of a set of pieces
  // apparently it's important that this be declared after Game.CYAN is declared above :)
  // this is not private because we show the maximizer piece at game start
  val maximizer: Box = GamePieces.bigBox

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

  // todo - normalize each optfactor from 0 to 1.  then multiply by a weight (established at the outset)
  //        then sum the optfactors and compare to previous sum to see if you can mimic the existing
  //        comparison - once that's done then drop the old comparison as you now have a mechanism that
  //        can scale to examining future states and also doing supervised learning

  private val totalPositions = math.pow(Board.BOARD_SIZE, 2).toInt

  val fullSpecification = ListMap(

    // specification provides the ordering of the optimization as well as whether a particular optimization is maximized or minimized
    // you'll need to update board.results, Simulation.compare, class Specification, and MINIMUM_SPEC_LENGTH if you change the length of the fullSpecification Array
    // other than that, you can rearrange rows in the specification, or turn entries off or on at will
    // much more flexible than it used to be

    // todo - run through all specification combinations of off and on and run 1000? games on each to see which specification is the best
    maximizerCountName -> OptimizationFactor(enabled = true, maximizerCountName, maximize, 0.0, 1.0, math.pow((Board.BOARD_SIZE - maximizer.cols + 1), 2).toInt, "maximizer", "positions in which a 3x3 piece can fit"),
    occupiedCountName -> OptimizationFactor(enabled = true, occupiedCountName, minimize, 0.0, 100, totalPositions, "occupied", "occupied positions"),
    fourNeighborsName -> OptimizationFactor(enabled = true, fourNeighborsName, minimize, 0.0, 100, totalPositions / 2, "4 neighbors", "number of positions surrounded on all 4 sides"),
    threeNeighborsName -> OptimizationFactor(enabled = true, threeNeighborsName, minimize, 0.0, 100, totalPositions / 2, "3 neighbors", "number of positions surrounded on 3 of 4 sides"),
    maxContiguousName -> OptimizationFactor(enabled = true, maxContiguousName, maximize, 0.0, 100, Board.BOARD_SIZE, "contiguous open lines", "number of lines (either horizontal or vertical) that are open and contiguous"),

    // i'm really not sure that 60 is the maximum number of two neighbors that can be created on a board
    // but i couldn't find another solution that was better
    twoNeighborsName -> OptimizationFactor(enabled = false, twoNeighborsName, minimize, 0.0, 100, (totalPositions * .6).toInt, "2 neighbors", "number of positions surrounded on 2 of 4 sides"),
    openLinesName -> OptimizationFactor(enabled = false, openLinesName, maximize, 0.0, 100, Board.BOARD_SIZE + Board.BOARD_SIZE - 1, "open rows + cols", "count of open rows plus open columns")

  )

  // by default return the full specification
  def apply(): Specification = {
    val filteredSpec: Specification = Specification(fullSpecification.filter(opt => opt._2.enabled))
    // simplify specifications by getting rid of case match/tuple sorting, etc.
    require(filteredSpec.length >= MINIMUM_SPEC_LENGTH, "because of the old comparison by sorting model, specifications have a case match to different tuple sizes and the minimum size is: " + MINIMUM_SPEC_LENGTH)
    filteredSpec
  }

  /**
   * used for testing purposes (for now) and eventually for an exhaustive run
   * through of all possible permutations and combinations of specifications
   * @return
   */
  def getAllSpecifications: Array[Array[Array[OptimizationFactor]]] = {

    val r = (1 to fullSpecification.size /*length*/ ).toArray

    val combinations = for { i <- r } yield fullSpecification.values.toArray.combinations(i).toArray

    val result = for {
      comboArray <- combinations
      combo <- comboArray
    } yield combo.permutations.toArray

    result

  }

}