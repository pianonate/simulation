/**
 * Created by nathan on 1/6/17.
 * created for the purpose of simplifying new algo choices by providing a specification in the Simulation object
 * that all simulations use to do the compare and to do a show
 */
import scala.collection.immutable.BitSet

/**
 * a full simulation includes a List of pieces at place locations
 * plus a boolean indicating whether any lines were cleared
 * the latter is useful as we don't need to run other permutations
 * of the ordering of these pieces if no lines are cleared
 * makes the game run faster
 * @param piece - the piece that is placed
 * @param loc - the lcoation at which the piece is place (row, col)
 * @param clearedLines  - were any lines cleared?
 */
case class PieceLocCleared(
  piece:        Piece,
  loc:          Loc,
  clearedLines: Boolean
)

/**
 * SimulationInfo is used to display resuls at the end of each round
 * @param pieces - the list of pieces that were placed this round
 * @param simulationCount - how many combinations of locations were simulated for these three pieces
 * @param best - the best simulation as determined by the Specification
 * @param worst - the worst simulation as determined by the Specification
 * @param elapsed - how much time did it take to run all of the simulationCount simulations
 */
case class SimulationInfo(
  pieces:          List[Piece],
  simulationCount: Int,
  best:            Simulation,
  worst:           Simulation,
  elapsed:         Long
)

/**
 * A single Simulation is the list of the PieceLocCleared (defined above) plus the resultant Board
 * A Simulation also stashes the results of the simulation (as determined by the Specification)
 *
 * @param plcList - PieceLocCleared list
 * @param board - the resultant Board after placing all pieces in PieceLocCleared.
 */
case class Simulation(plcList: List[PieceLocCleared], board: Board, specLength: Int) extends Ordered[Simulation] {

  override def toString: String = this.plcList.map(plc => plc.piece.name).mkString(", ") // visible in debugger

  // used to group Simulations
  // if pieceCount is less than 3 then GameOver man!
  val pieceCount: Int = plcList.length

  // results
  val results: Array[Int] = board.results

  //todo see if youc an quickly build a bitset and use it as a key or try scalacache and guava
 /// val bs: BitSet = board.grid.bitSet


  // wayyyyy toooooo slowwwww
  // Simulation.hashMap += (bs -> this)

  def compare(that: Simulation): Int = {

    // the following provides tuple ordering to ordered to make the tuple comparison work
    import scala.math.Ordered.orderingToOrdered

    // any result to be maximized is negated so that it will work with default Int sort

    val a = this.results
    val b = that.results

    // tell Scalariform not to bother to change my awesome formatting
    // it would be nice if Scalariform did this by default...
    // format: OFF
    specLength match {
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

object Simulation {

  val aSim = new Array[Simulation](1000000)

}