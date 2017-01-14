/**
 * Created by nathan on 1/6/17.
 * created for the purpose of simplifying new algo choices by providing a specification in the Simulation object
 * that all simulations use to do the compare and to do a show
 */
case class PieceLocCleared(
  piece:        Piece,
  loc:          Loc,
  clearedLines: Boolean
)

case class SimulationInfo(
  pieces:          List[Piece],
  simulationCount: Int,
  best:            Simulation,
  worst:           Simulation,
  elapsed:         Long,
  perSecond:       Long
)

case class Simulation(plcList: List[PieceLocCleared], board: Board) extends Ordered[Simulation] {

  override def toString: String = this.plcList.map(plc => plc.piece.name).mkString(", ") // visible in debugger

  val pieceCount: Int = plcList.length
  val results: Array[Int] = board.results

  // format: OFF
  def compare(that: Simulation): Int = {

    // the following provides tuple ordering to ordered to make the tuple comparison work
    import scala.math.Ordered.orderingToOrdered

    // any result to be maximized is negated so that it will work with default Int sort

    val a = this.results
    val b = that.results

    Simulation.specification.length match {
      case 1 => a(0)                                       compare b(0)
      case 2 => (a(0), a(1))                               compare (b(0), b(1))
      case 3 => (a(0), a(1), a(2))                         compare (b(0), b(1), b(2))
      case 4 => (a(0), a(1), a(2), a(3))                   compare (b(0), b(1), b(2), b(3))
      case 5 => (a(0), a(1), a(2), a(3), a(4))             compare (b(0), b(1), b(2), b(3), b(4))
      case 6 => (a(0), a(1), a(2), a(3), a(4), a(5))       compare (b(0), b(1), b(2), b(3), b(4), b(5))
      case 7 => (a(0), a(1), a(2), a(3), a(4), a(5), a(6)) compare (b(0), b(1), b(2), b(3), b(4), b(5), a(6))
    }

    // format: ON

  }
}

case class OptimizationFactor(
  enabled:     Boolean,
  fieldName:   String,
  minimize:    Boolean,
  resultLabel: String,
  explanation: String
)

object Simulation {

  // your system has some cred if it is doing more than this number of simulations / second
  private val BYATCH_THRESHOLD = 550000

  // for readability
  private val minimize = true
  private val maximize = false

  private val on = true
  private val off = false

  val occupiedCountName = "occupiedCount"
  val maximizerCountName = "maximizerCount"
  val fourNeighborsName = "fourNeighbors"
  val threeNeighborsName = "threeNeighbors"
  val islandMaxName = "islandMax"
  val maxContiguousName = "openContiguous"
  val openLinesName = "openLines"

  private val fullSpecification = Array(

    // specification provides the ordering of the optimization as well as whether a particular optimization is maximized or minimized
    // you'll need to update board.results and Simulation.compare if you change the length of the fullSpecification Array
    // other than that, you can rearrange rows in the specification, or turn entries off or on at will
    // much more flexible than it used to be

    // todo - run through all specification combinations of off and on and run 1000? games on each to see which specification is the best
    //        after a thousand games

    OptimizationFactor(on, occupiedCountName, minimize, "occupied", "occupied positions"),
    OptimizationFactor(on, maximizerCountName, maximize, "maximizer", "positions in which a 3x3 piece can fit"),
    OptimizationFactor(on, fourNeighborsName, minimize, "4 neighbors", "number of positions surrounded on all 4 sides"),
    OptimizationFactor(on, threeNeighborsName, minimize, "3 neighbors", "number of positions surrounded on 3 of 4 sides"),
    OptimizationFactor(on, maxContiguousName, maximize, "contiguous open lines", "number of lines (either horizontal or vertical) that are open and contiguous"),
    OptimizationFactor(off, openLinesName, maximize, "open Rows & Cols", "count of open rows plus open columns"),
    OptimizationFactor(off, islandMaxName, maximize, "islandMax", "largest number of connected, unoccupied positions")


  )

  val specification: Array[OptimizationFactor] = fullSpecification.filter(_.enabled)

  def getOptimizationFactorExplanations: String = {
    // used by showGameStart
    specification.map(optFactor => "* " + optFactor.resultLabel + " - " + optFactor.explanation).mkString("\n")
  }

  private val resultFormat = "%2d"
  private val resultParenFormat = " (" + resultFormat + ")"
  private val resultLabelFormat = " %s: "

  def greenifyResult(isGreen: Boolean, value: Int, valFormat: String, label: String, labelFormat: String): String = {
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

    def getResultString(simulationResult: SimulationInfo, simulationIndex:Int): String = {

      def handleOptFactor(optFactor: OptimizationFactor, bestVal: Int, worstVal: Int, topValIndex: Int): String = {
        val topVal = bestOfAll(topValIndex)
        greenifyResult(bestVal == topVal, bestVal, resultFormat, optFactor.resultLabel, resultLabelFormat) + greenifyResult(worstVal == topVal, worstVal, resultParenFormat, "", "")
      }

      val best = simulationResult.best.results

      val worst = simulationResult.worst.results

      (simulationIndex + 1)  + ": " + simulationResult.pieces.map(_.name).mkString(", ") + " -" +
        specification
        .zip(best).zip(worst).zipWithIndex
        .map(tup => (tup._1._1._1, tup._1._1._2, tup._1._2, tup._2))
        .map(tup => handleOptFactor(tup._1, tup._2, tup._3, tup._4))
        .mkString

    }

    def getPerformanceString(result: SimulationInfo): String = {
      val largeValuesFormat = "%,5d"

      val simulationCountString = largeValuesFormat.format(result.simulationCount)

      val durationString = largeValuesFormat.format(result.elapsed) + "ms"
      val perSecondString = largeValuesFormat.format(result.perSecond)

      " - simulations: " + simulationCountString +
        " in " + durationString + " (" + perSecondString + "/second" +
        (if (result.perSecond > BYATCH_THRESHOLD) " b-yatch" else "") +
        ")"
    }

    simulationResults.zipWithIndex.map { tup =>
      val r = tup._1
      val index = tup._2

      val s = getResultString(r, index) + getPerformanceString(r)

      // underline is for closers (Glengarry Glen Ross)
      if (r.best == chosen) {
        val parts = s.splitAt(s.indexOf(" ")+1)
        parts._1 + Game.UNDERLINE + parts._2.split(Game.ESCAPE).mkString(Game.UNDERLINE + Game.ESCAPE) + Game.SANE
      }
      else
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

    specification
      .zip(boardResult)
      .map(tup => handleOptFactor(tup._1, tup._2))
      .mkString

  }

  // one of the optimizations is to ensure that the maximum number of
  // maximum pieces can fit on a board from all the boards simulated in the permutation of a set of pieces
  // apparently it's important that this be declared after Game.CYAN is declared above :)
  // this is not private because we show the maximizer piece at game start
  val maximizer: Box = Pieces.bigBox

}