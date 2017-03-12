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
 * SimulationInfo is used to display results at the end of each round
 * @param pieces - the list of pieces that were placed this round
 * @param simulatedCount - how many combinations of locations were simulated for these three pieces
 * @param best - the best simulation as determined by the Specification
 * @param elapsedMs - how much time did it take to run all of the simulationCount simulations
 */
case class SimulationInfo(
  pieces:             List[Piece],
  simulatedCount:     Int,
  unsimulatedCount:   Int,
  best:               Simulation,
  rcChangedCountBest: Int,
  elapsedMs:          Int
)

/**
 * A single Simulation is the list of the PieceLocCleared (defined above) plus the resultant Board
 * A Simulation also stashes the results of the simulation (as determined by the Specification)
 *
 * @param plcList - PieceLocCleared list
 * @param board - the resultant Board after placing all pieces in PieceLocCleared.
 */
case class Simulation(plcList: List[PieceLocCleared], board: Board, id: Int, pieceCount: Int) {

  override def toString: String = this.plcList.map(plc => plc.piece.name).mkString(", ") // visible in debugger

  // used to group Simulations
  // if pieceCount is less than 3 then GameOver man!
  //  val pieceCount: Int = plcList.length

  // pieceCount to weighted sum so Best choice always considers a solution that includes all pieces
  // to be better than a solution with less pieces
  // this only works because the weighted sum is normalized to range from 0 to 1 so can never
  // cause a 2 piece solution to be better than a 3 piece solution
  val weightedSum: Double = pieceCount + board.boardScore.weightedSum

}

