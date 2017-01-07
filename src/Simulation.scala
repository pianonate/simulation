import com.sun.javaws.exceptions.InvalidArgumentException

/**
 * Created by nathan on 1/6/17.
 * created for the purpose of simplifying new algo choices by providing a specification in the Simulation object
 * that all simulations use to do the compare and to do a show
 */
case class PieceLocCleared(piece: Piece, loc: (Int, Int), clearedLines: Boolean)

case class Simulation(plcList: List[PieceLocCleared], board: Board) extends Ordered[Simulation] {

  val pieceCount: Int = plcList.length

  val boardCount: Int = board.occupiedCount
  val maximizerCount: Int = board.legalPlacements(Simulation.maximizer).length
  val (openLines: Int, openContiguous: Int) = board.openLines
  val islandMax: Int = 0 // board.islandMax
  val neighborCounts: Array[Int] = board.neighborCount
  val fourNeighbors: Int = neighborCounts(4)
  val threeNeighbors: Int = neighborCounts(3)

  // the following provides tuple ordering to ordered to make the tuple comparison work
  import scala.math.Ordered.orderingToOrdered

  override def toString: String = this.plcList.map(plc => plc.piece.name).mkString(", ")

  // format: OFF
  def compare(that: Simulation): Int = {

    //val compareTuples = Simulation.getCompareTuples(this, that)

    (this.boardCount,  that.maximizerCount, this.fourNeighbors, this.threeNeighbors, that.openContiguous/*, that.islandMax, that.openLines*/)
      .compare(that.boardCount,  this.maximizerCount, that.fourNeighbors, that.threeNeighbors, this.openContiguous/*, this.islandMax, this.openLines*/)

    // format: ON

  }

}

object Simulation {
/*
  Simulation.getClass().getField("boardCount").get

  private val minimize = true
  private val maximize = false
  private val boardCountName = "boardCount"
  private val maximizerCountName = "maximizerCount"
  private val fourNeighborsName = "fourNeighbors"
  private val threeNeighborsName = "threeNeighbors"
  private val openContiguousName = "openContiguous"
  private val islandMaxName = "islandMax"
  private val openLinesName = "openLines"

  // specification provides the ordering of the optimization as well as whether a particular optimization is maximized or minimized
  // it's hacky - but for now, if you change the numbe of rows in the specification, you have to change the
  // number of values in the tuples returned from getCompareTuples and the cases in getCompareValue
  private val specification = List(
    (boardCountName, minimize),
    (maximizerCountName, maximize),
    (fourNeighborsName, minimize),
    (threeNeighborsName, minimize),
    (openContiguousName, maximize)
  )

  private def getCompareTuples(thisSimulation: Simulation, thatSimulation: Simulation): ((Int, Int, Int, Int, Int), (Int, Int, Int, Int, Int)) = {


    def getUseSimulation(firstTuple:Boolean, minimize:Boolean):Simulation = {
      (firstTuple, minimize) match {
        case (true, true) => thisSimulation
        case (true, false) => thatSimulation
        case (false, true) => thatSimulation
        case (false, false) => thisSimulation
      }
    }
    def getCompareValue(spec:(Simulation,String)): Int = {

      spec match {
        case (sim, arg) if (arg == boardCountName)     => sim.boardCount
        case (sim, arg) if (arg == maximizerCountName) => sim.maximizerCount
        case (sim, arg) if (arg == fourNeighborsName)  => sim.fourNeighbors
        case (sim, arg) if (arg == threeNeighborsName) => sim.threeNeighbors
        case (sim, arg) if (arg == openContiguousName) => sim.openContiguous
        case (sim, arg) if (arg == islandMaxName)      => sim.islandMax
        case (sim, arg) if (arg == openLinesName)      => sim.openLines
        case (sim, arg) => throw new InvalidArgumentException(Array("Unknown parameter passed to getCompareVaue: " + arg))
      }
    }

    val lowerList = specification.map(arg => (getUseSimulation(arg._2, true), arg._1)).map(getCompareValue)
    val upperList = specification.map(arg => (getUseSimulation(arg._2, false), arg._1)).map(getCompareValue)

    (
      (lowerList(0), lowerList(1), lowerList(2), lowerList(3), lowerList(4)),
      (upperList(0), upperList(1), upperList(2), upperList(3), upperList(4))
    )

  }*/

  // one of the optimizations is to ensure that the maximum number of
  // maximum pieces can fit on a board from all the boards simulated in the permutation of a set of pieces
  // apparently it's important that this be declared after Game.CYAN is declared above :)
  // this is not private because we show the maximizer piece at game start
  val maximizer = new Box("Maximizer", Game.CYAN, 3, 0)

}