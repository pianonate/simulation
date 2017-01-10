/**
 * Created by nathan on 1/6/17.
 * created for the purpose of simplifying new algo choices by providing a specification in the Simulation object
 * that all simulations use to do the compare and to do a show
 */
case class PieceLocCleared(piece: Piece, loc: (Int, Int), clearedLines: Boolean)

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

  def getSimulationResultsString(worst: Option[Array[Int]]): String = Simulation.getResultsString(this.results, worst)

}

case class OptimizationFactor(
  enabled:      Boolean,
  fieldName:    String,
  minimize:     Boolean,
  resultsLabel: String,
  explanation:  String

)

object Simulation {

  // for readability
  private val minimize = true
  private val maximize = false

  private val on = true
  private val off = false

  val occupiedCountName = "occupiedCount"
  val maximizerCountName = "maximizerCount"
  val fourNeighborsName = "fourNeighbors"
  val threeNeighborsName = "threeNeighbors"
  val openContiguousName = "openContiguous"
  val islandMaxName = "islandMax"
  val openLinesName = "openLines"

  // specification provides the ordering of the optimization as well as whether a particular optimization is maximized or minimized
  // you'll need to update getCompareTupleArray and Simulation.compare if you change the length of the specification
  // other than that, you can rearrange rows in the specification, or turn entries off or on at will
  // much more flexible than it used to be
  private val fullSpecification = Array(
    OptimizationFactor(on, occupiedCountName, minimize, "occupied", "occupied positions"),
    OptimizationFactor(on, maximizerCountName, maximize, "maximizer", "positions in which a 3x3 piece can fit"),
    OptimizationFactor(on, fourNeighborsName, minimize, "4 neighbors", "number of positions surrounded on all 4 sides"),
    OptimizationFactor(on, threeNeighborsName, minimize, "3 neighbors", "number of positions surrounded on 3 of 4 sides"),
    OptimizationFactor(on, openContiguousName, maximize, "contiguous open lines", "number of lines (either horizontal or vertical) that are open and contiguous"),
    OptimizationFactor(off, openLinesName, maximize, "openRowsCols", "count of open rows plus open columns"),
    OptimizationFactor(off, islandMaxName, maximize, "islandMax", "largest number of connected, unnoccupied positions")
  )

  val specification: Array[OptimizationFactor] = fullSpecification.filter(_.enabled)
  private val specificationMinimized = specification.map(_.minimize)

  def getOptimizationFactorExplanations: String = {
    // used by showGameStart
    specification.map(optFactor => "* " + optFactor.resultsLabel + " - " + optFactor.explanation).mkString("\n")
  }

  // todo - collect all results and do a columnwise coloration of best and worst rather than one at a time
  def getResultsString(best: Array[Int], worst: Option[Array[Int]] = None): String = {
    // this is not implemented on simulation as it can be called from a simulation result or
    // from a board placement result during the actual placing of pieces post-simulation
    // we keep board placement results separate on the one board that the whole game runs on
    // so that we can compare expected results from a simulation with actual results on the board
    // additionally, this mechanism allows us to display line clearing.
    // all of the above is to say, this results string is fairly complex because of keeping these 
    // things separate.  for now, this is acceptable
    // at least the results are guided by the specification.  Previous instances were not and 
    // there was a lot of duplication and gnashing of teeth

    def greenify(isGreen: Boolean, value: Int, valFormat: String, label: String, labelFormat: String): String = {
      // maximized results are negated to work with Simulation.compare so
      // as not to have to jump through hoops in that method
      // use math.abs to show them to the user in a way that makes sense
      val result = valFormat.format(math.abs(value))
      labelFormat.format(label) + (if (isGreen) Game.GREEN + result + Game.SANE else Game.RED + result + Game.SANE)
    }

    val openFormat = "%2d"
    val parenFormat = " (" + openFormat + ")"
    val labelFormat = " %s: "
    val longLabelFormat = "     " + labelFormat

    var first = true

    def handleOptFactor(optFactor: OptimizationFactor, bestVal: Int, worstValOption: Option[Int]): String = {

      worstValOption match {
        case w: Some[Int] =>

          val worstVal = w.get
          val greenBest = bestVal <= worstVal // if (optFactor.minimize) bestVal <= worstVal else bestVal >= worstVal
          val greenWorst = worstVal <= bestVal // if (optFactor.minimize) bestVal >= worstVal else bestVal <= worstVal

          greenify(greenBest, bestVal, openFormat, optFactor.resultsLabel, labelFormat) + greenify(greenWorst, worstVal, parenFormat, "", "")

        case None =>

          val theLabel = if (first) { first = false; labelFormat } else longLabelFormat

          val green = true
          greenify(green, bestVal, openFormat, optFactor.resultsLabel, theLabel)
      }
    }

    val worstValues = worst match {
      case w: Some[Array[Int]] => w.get.map(i => Some(i))
      case None                => specification.map(_ => None)
    }

    specification
      .zip(best.zip(worstValues))
      .map(tup => (tup._1, tup._2._1, tup._2._2))
      .map(tup => handleOptFactor(tup._1, tup._2, tup._3))
      .mkString

  }

  // one of the optimizations is to ensure that the maximum number of
  // maximum pieces can fit on a board from all the boards simulated in the permutation of a set of pieces
  // apparently it's important that this be declared after Game.CYAN is declared above :)
  // this is not private because we show the maximizer piece at game start
  val maximizer = new Box("Maximizer", Game.CYAN, 3, 0)

}