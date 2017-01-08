import com.sun.javaws.exceptions.InvalidArgumentException

/**
 * Created by nathan on 1/6/17.
 * created for the purpose of simplifying new algo choices by providing a specification in the Simulation object
 * that all simulations use to do the compare and to do a show
 */
case class PieceLocCleared(piece: Piece, loc: (Int, Int), clearedLines: Boolean)

case class Simulation(plcList: List[PieceLocCleared], board: Board) extends Ordered[Simulation] {

  val pieceCount: Int = plcList.length

  val occupiedCount: Int = board.occupiedCount
  val maximizerCount: Int = board.maximizerCount

  val (openLines: Int, openContiguous: Int) = board.getOpenAndContiguousLines

  val islandMax: Int = 0 // board.islandMax

  val neighborCounts: Array[Int] = board.neighborCount
  val fourNeighbors = neighborCounts(4)
  val threeNeighbors = neighborCounts(3)

  // the following provides tuple ordering to ordered to make the tuple comparison work
  import scala.math.Ordered.orderingToOrdered

  override def toString: String = this.plcList.map(plc => plc.piece.name).mkString(", ")

  // format: OFF
  def compare(that: Simulation): Int = {

/*    // for now, not using dynamic tuple creation as reflection is too slow for the compare method
    // vaulting compare to ~13% of overall execution time which would slow things down to much
    // even though it would be very convenient if we could dynamically construct
    // the calls here
    val firstNew = (
      Simulation.getCompareValue(0,true,this,that),
      Simulation.getCompareValue(1,true,this,that),
      Simulation.getCompareValue(2,true,this,that),
      Simulation.getCompareValue(3,true,this,that),
      Simulation.getCompareValue(4,true,this,that)
    )

    val secondNew = (
      Simulation.getCompareValue(0,false,this,that),
      Simulation.getCompareValue(1,false,this,that),
      Simulation.getCompareValue(2,false,this,that),
      Simulation.getCompareValue(3,false,this,that),
      Simulation.getCompareValue(4,false,this,that)
    )

    val firstTuple = (this.occupiedCount, that.maximizerCount, this.fourNeighbors, this.threeNeighbors, that.openContiguous /*, that.islandMax, that.openLines )
    val secondTuple = (that.occupiedCount, this.maximizerCount, that.fourNeighbors, that.threeNeighbors, this.openContiguous , that.islandMax, that.openLines )

    assert(firstTuple==firstNew, "first doesn't match")
    assert(secondTuple==secondNew, "second doesn't match")


    firstNew compare secondNew */*/

              (this.occupiedCount, that.maximizerCount, this.fourNeighbors, this.threeNeighbors, that.openContiguous /*, that.islandMax, that.openLines */ )
      .compare(that.occupiedCount, this.maximizerCount, that.fourNeighbors, that.threeNeighbors, this.openContiguous /*, that.islandMax, that.openLines */ )

    // format: ON

  }

  def getSimulationResultsString(worst: Option[Simulation] = None): String = Simulation.getSpecResultsString(this, worst)

}

private case class Spec(
  val fieldName: String,
  minimize: Boolean,
  resultsLabel: String,
  explanation: String
)

object Simulation {

  private val minimize = true
  private val maximize = false

  // extract to a utility object? maybe...
  private def invokeGet(instance: Simulation, fieldName: String) = {
    try {
      val field = instance.getClass.getDeclaredField(fieldName)
      field.setAccessible(true)
      field.get(instance).asInstanceOf[Int]
    } catch {
      case e: Throwable => throw new java.lang.IllegalStateException("invokeGet couldn't resolve: " + fieldName)
      case _: Throwable => throw new IllegalArgumentException("fuck")
    }
  }

  // specification provides the ordering of the optimization as well as whether a particular optimization is maximized or minimized
  private val specification = Array(
    Spec("occupiedCount", minimize, "occupied", "occupied positions"),
    Spec("maximizerCount", maximize, "maximizer", "positions in which a 3x3 piece can fit"),
    Spec("fourNeighbors", minimize, "4 neighbors", "number of positions surrounded on all 4 sides"),
    Spec("threeNeighbors", minimize, "3 neighbors", "number of positions surrounded on 3 of 4 sides"),
    Spec("openContiguous", maximize, "contiguous open lines", "number of lines (either horizontal or vertical) that are open and contiguous") /*,
    Spec("islandMax", maximize, "islandMax", "largest number of connected, unnoccupied positions"),
    Spec("openLines", maximize, "openRowsCols", "count of open rows plus open columns")*/
  )

  // used by showGameStart
  def getSpecDescriptions: String = {
    specification.map(spec => "* " + spec.resultsLabel + " - " + spec.explanation).mkString("\n")

  }

  def getSpecResultsString(instance: Simulation, worst: Option[Simulation] = None): String = {

    def greenify(isGreen: Boolean, value: Int, valFormat: String, label: String, labelFormat: String) = {
      val result = valFormat.format(value)
      labelFormat.format(label) + (if (isGreen) Game.GREEN + result + Game.SANE else result)
    }

    val openFormat = "%2d"
    val parenFormat = " (" + openFormat + ")"
    val labelFormat = " %s: "
    val longLabelFormat = "     " + labelFormat

    var first = true

    def handleSpec(spec: Spec): String = {

      val bestVal = invokeGet(instance, spec.fieldName)

      worst match {
        case w: Some[Simulation] => {

          val worstVal = invokeGet(w.get, spec.fieldName)
          val greenBest = if (spec.minimize) bestVal < worstVal else bestVal > worstVal
          val greenWorst = if (spec.minimize) bestVal > worstVal else bestVal < worstVal

          greenify(greenBest, bestVal, openFormat, spec.resultsLabel, labelFormat) +
            greenify(greenWorst, worstVal, parenFormat, "", "")

        }
        case None => {

          val theLabel = if (first) { first = false; labelFormat } else longLabelFormat

          greenify(true, bestVal, openFormat, spec.resultsLabel, theLabel)

        }
      }
    }

    specification.map(handleSpec).mkString

  }

  // could be used by Compare but reflection is too slow...
  // if you can find a way around this then think about using it
  private def getCompareValue(index: Int, first: Boolean, thisSimulation: Simulation, thatSimulation: Simulation) = {

    val spec = specification(index)
    val (name, minimize) = (spec.fieldName, spec.minimize)

    (first, minimize) match {
      case (true, true) => invokeGet(thisSimulation, name)
      case (true, false) => invokeGet(thatSimulation, name)
      case (false, true) => invokeGet(thatSimulation, name)
      case (false, false) => invokeGet(thisSimulation, name)
    }

  }

  // one of the optimizations is to ensure that the maximum number of
  // maximum pieces can fit on a board from all the boards simulated in the permutation of a set of pieces
  // apparently it's important that this be declared after Game.CYAN is declared above :)
  // this is not private because we show the maximizer piece at game start
  val maximizer = new Box("Maximizer", Game.CYAN, 3, 0)

}