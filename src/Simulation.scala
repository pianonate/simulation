/**
 * Created by nathan on 1/6/17.
 * created for the purpose of simplifying new algo choices by providing a specification in the Simulation object
 * that all simulations use to do the compare and to do a show
 */

/**
 * a full simulation includes a List of pieces at place locations
 * plus a boolean indicating whether any lines were cleared
 * the latter is useful as we don't need to run other permutations
 * of the ordering of these pieces if no lines are cleared
 * makes the game run faster
 * @param piece - the piece that is placed
 * @param loc - the location at which the piece is place (row, col)
 * @param clearedLines  - were any lines cleared?
 */
case class PieceLocCleared(
  piece:        Piece,
  loc:          Loc,
  clearedLines: Boolean
)

/**
 * A single Simulation is the list of the PieceLocCleared (defined above) plus the resultant Board
 * A Simulation also stashes the results of the simulation (as determined by the Specification)
 *
 * @param plcList - PieceLocCleared list
 * @param board - the resultant Board after placing all pieces in PieceLocCleared.
 */
case class Simulation(plcList: List[PieceLocCleared], board: Board, specLength: Int, id:Int) extends Ordered[Simulation] {

  import Implicits._
  override def toString: String = this.plcList.map(plc => plc.piece).label // visible in debugger

  // used to group Simulations
  // if pieceCount is less than 3 then GameOver man!
  val pieceCount: Int = plcList.length

  // results
  val results: Array[Int] = board.results
  val emptyResults:Array[Int] = board.emptyResults

  //todo see if you can quickly build a bitset and use it as a key or try scalacache and guava

  def compare(that: Simulation): Int = {

    // if the PieceLocCleared length is less than the comparison
    // then this instance is _worse_ than the other - indicate
    // that by returning a 1 and bailing
    if ((this.plcList.length compare that.plcList.length) < 0)
      1
    else {

      // the following provides tuple ordering to ordered to make the tuple comparison work
      import scala.math.Ordered.orderingToOrdered

      // any result to be maximized is negated so that it will work with default Int sort

      val a = this.results
      val b = that.results

      // tell Scalariform not to bother to change my awesome formatting
      // it would be nice if Scalariform did this by default...
      // format: OFF
      specLength match {
        case 5 => (a(0), a(1), a(2), a(3), a(4))             compare (b(0), b(1), b(2), b(3), b(4))
        case 6 => (a(0), a(1), a(2), a(3), a(4), a(5))       compare (b(0), b(1), b(2), b(3), b(4), b(5))
        case 7 => (a(0), a(1), a(2), a(3), a(4), a(5), a(6)) compare (b(0), b(1), b(2), b(3), b(4), b(5), a(6))
      }
      // format: ON
    }
  }
}

object Simulation {

  val aSim = new Array[Simulation](1000000)

}