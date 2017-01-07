/**
 * Created by nathan on 1/6/17.
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

    (this.boardCount,  that.maximizerCount, this.fourNeighbors, this.threeNeighbors, that.openContiguous/*, that.islandMax, that.openLines*/)
      .compare(that.boardCount,  this.maximizerCount, that.fourNeighbors, that.threeNeighbors, this.openContiguous/*, this.islandMax, this.openLines*/)

    // format: ON

  }

}

object Simulation {


  // one of the optimizations is to ensure that the maximum number of
  // maximum pieces can fit on a board from all the boards simulated in the permutation of a set of pieces
  // apparently it's important that this be declared after Game.CYAN is declared above :)
  // this is not private because we show the maximizer piece at game start
  val maximizer = new Box("Maximizer", Game.CYAN, 3, 0)

}