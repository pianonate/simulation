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
import Specification._
import scala.collection.immutable.{Iterable, ListMap}

case class Specification(spec: ListMap[String, OptimizationFactor]) {

  val length: Int = spec.size

  //def apply(i: Int): OptimizationFactor = weightedSpec(i)
  def apply(s: String): OptimizationFactor = weightedSpec(s)

  private val weightedSpec: ListMap[String, OptimizationFactor] = {

    // divide each by 100 and then the next one divides previous by 100
    // val weights = Array.tabulate(length)(n => 1 / (math.pow(10, n) * math.pow(10, n)))
    val weights = spec.values.map(each => each.initialWeightFactor).scanLeft(1.0)((a, b) => a / b).tail

    // the idea to normalize weights this way is that when you multiply weights times the
    // the normalized values derived from creating the scores, we will get a distribution from 0 to 1
    // Brendan's idea and I like it.  Makes it easy to grok the scores
    // however, just calculating it by mapping each weight over the sum caused a bug
    // when the board clears, the scores are perfect - all normalize to 1.0
    // but when multiplying by normalized weights, we ended up with this number:
    // 1.0000000000000002
    // which is larger than one - and unnacceptable :)
    // so the answer is to take the first n normalized weights and to calculate the last one by
    // subtracting the sum of the first n from 1.  And here it is:
    val sumOfWeights = weights.sum
    val initialNormalized = weights.init.map(_ / sumOfWeights)
    val last: Double = 1.0 - initialNormalized.sum
    val normalizedWeights = initialNormalized ++ Iterable(last)

    val weighted = spec.zip(normalizedWeights).map {
      case (specEntry, weight) =>
        specEntry match {
          case (key, opt) => (key, OptimizationFactor(opt.enabled, opt.name, opt.minimize, weight, opt.initialWeightFactor, opt.maxVal, opt.label, opt.explanation))
        }
    }

    weighted
  }

  // provided so that the getNamedResult function in boardScore can iterate over a while loop
  // as using named keys into the ListMap along with apply() access is VERY SLOW
  val optimizationFactors: Array[OptimizationFactor] = weightedSpec.values.toArray

  // initialize named optimization factors
  private def getOptimizationFactor(name: String): OptimizationFactor = {
    // a filtered spec is passed in - non non-enabled specifications are allowed
    // because of this, the direct values we provide on this Specification instance
    // need to provide a default for non-enabled values
    if (weightedSpec.contains(name))
      weightedSpec(name)
    else
      OptimizationFactor(enabled = false, name, minimize = false, 0.0, 0.0, 0, "", "")
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

  def getSimulationResultsString(simulationResults: List[SimulationInfo], chosen: Simulation, showWorst: Boolean, bullShit:String): String = {
    // todo - add in simulations, skipped simulations, rounds cleared, persecond information
    // todo - allow for outputting worst
    // todo - generate json for both brendan and lior
    // todo - loc for each permutation piece placement
    /*(
      simulationResults.map(_.best.weightedSum) ++ // best result
        (if (showWorst) simulationResults.map(_.worst.get.weightedSum) else List[Array[Double]]()) // worst results (or empty if not showing anything)
      ).transpose.map(_.max).toArray // sort out the best of all...*/

    val wrappedBullShit = bullShit.wrap(piecePrefixLength, GamePieces.tallestPiece+1, StringFormats.BRIGHT_MAGENTA)

    val piecesString = simulationResults
      .zipWithIndex
      .map { case (result, index) => result.pieces.permutationPiecesHeader(index) }
      .spreadHorizontal(startAt = /*piecePrefixLength*/0, bracketWith = StringFormats.VERTICAL_LINE + " ", separator = (StringFormats.VERTICAL_LINE + " "))

    val newPiecesString = wrappedBullShit.splice(piecesString.split("\n"))

    // get the scores from the simulationInfo
    // transpose them to get all the same score values on the same row
    val scores: List[List[ScoreComponent]] = simulationResults.map(info => info.best.board.scores).transpose

    val horizontal = StringFormats.HORIZONTAL_LINE.repeat(prefixPaddingLength) +
      (StringFormats.CROSS_PIECE + StringFormats.HORIZONTAL_LINE.repeat(columnHeader.length - 1)).repeat(simulationResults.length) +
      StringFormats.VERTICAL_AND_LEFT + "\n"

    val header = "score factor".leftAlignedPadded(maxOptFactorLabelLength).addColon +
      "weight".rightAlignedPadded(StringFormats.weightFormatLength + 2) + " " + StringFormats.VERTICAL_LINE +
      columnHeader.repeat(simulationResults.length) + "\n"

    val scoreString = scores.map(optScores =>
      optScores.head.label.leftAlignedPadded(maxOptFactorLabelLength).addColon + optScores.head.weight.yellowColoredWeightLabel + StringFormats.VERTICAL_LINE +
        optScores.map(score => " " + score.intValue.abs.optFactorLabel.rightAlignedPadded(columnPadding) + score.normalizedValue.label(2).rightAlignedPadded(columnPadding + 4) + "  " + score.weightedValue.yellowColoredWeightLabel)
        .mkString(StringFormats.VERTICAL_LINE)).mkString(StringFormats.VERTICAL_LINE + "\n") + StringFormats.VERTICAL_LINE

    val winner = simulationResults.map(_.best.weightedSum).max

    val weightedSumString = "sum".leftAlignedPadded(maxOptFactorLabelLength).addColon +
      scores.map(optScores => optScores.head.weight).sum.yellowColoredWeightLabel + StringFormats.VERTICAL_LINE +
      simulationResults.map { result =>

        val winner = result.best == chosen
        val sum = result.best.board.scores.map(scoreComponent => scoreComponent.weightedValue).sum.weightLabel
        val sumWithLabel = (if (winner) "winner: " + sum else sum).rightAlignedPadded(22 + StringFormats.weightFormatLength)
        if (winner) sumWithLabel.greenDigits else sumWithLabel.yellowDigits

      }.mkString(StringFormats.VERTICAL_LINE) +
      StringFormats.VERTICAL_LINE

    newPiecesString + "\n" + horizontal + header + horizontal + scoreString + "\n" + horizontal + weightedSumString
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
  name:                String,
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

  private val totalPositions = math.pow(Board.BOARD_SIZE, 2).toInt

  // todo - OptimizationFactor of all combinations of 3x3 1x5 and 5x1
  // todo - run through all specification combinations of off and on and run 1000? games on each to see which specification is the best

  val fullSpecification = ListMap(

    // specification provides the ordering of the optimization as well as whether a particular optimization is maximized or minimized
    // you'll need to update board.results, Simulation.compare, class Specification, and MINIMUM_SPEC_LENGTH if you change the length of the fullSpecification Array
    // other than that, you can rearrange rows in the specification, or turn entries off or on at will
    // much more flexible than it used to be

    maximizerCountName -> OptimizationFactor(enabled = true, maximizerCountName, maximize, 0.0, 1.0, math.pow(Board.BOARD_SIZE - maximizer.cols + 1, 2).toInt, "maximizer", "positions in which a 3x3 piece can fit"),
    occupiedCountName -> OptimizationFactor(enabled = true, occupiedCountName, minimize, 0.0, 100, totalPositions, "occupied", "occupied positions"),
    fourNeighborsName -> OptimizationFactor(enabled = true, fourNeighborsName, minimize, 0.0, 100, totalPositions / 2, "4 neighbors", "number of positions surrounded on all 4 sides"),
    threeNeighborsName -> OptimizationFactor(enabled = true, threeNeighborsName, minimize, 0.0, 100, totalPositions / 2, "3 neighbors", "number of positions surrounded on 3 of 4 sides"),
    maxContiguousName -> OptimizationFactor(enabled = true, maxContiguousName, maximize, 0.0, 100, Board.BOARD_SIZE, "connected open", "number of lines (either horizontal or vertical) that are open and contiguous"),

    // i'm really not sure that 60 is the maximum number of two neighbors that can be created on a board
    // but i couldn't find another solution that was better
    twoNeighborsName -> OptimizationFactor(enabled = true, twoNeighborsName, minimize, 0.0, 100, (totalPositions * .6).toInt, "2 neighbors", "number of positions surrounded on 2 of 4 sides"),
    openLinesName -> OptimizationFactor(enabled = false, openLinesName, maximize, 0.0, 100, Board.BOARD_SIZE + Board.BOARD_SIZE - 1, "open rows + cols", "count of open rows plus open columns")

  )

  // following are used to construct results
  val maxOptFactorLabelLength: Int = fullSpecification.values.map(_.label.length).max
  private val piecePrefixLength = 0.indexLabel.length + maxOptFactorLabelLength + StringFormats.weightFormatLength + 2
  private val prefixPaddingLength = maxOptFactorLabelLength + 2 + StringFormats.weightFormatLength + 3
  private val columnPadding = GamePieces.numPiecesInRound * 2
  private val columnHeader = "score".rightAlignedPadded(columnPadding + 1) +
    "normalized".rightAlignedPadded(columnPadding + 6) +
    "weighted".rightAlignedPadded(StringFormats.weightFormatLength + 2) +
    " " + StringFormats.VERTICAL_LINE

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