/**
 * Created by nathan on 12/9/16.
 * Board is the game Board plus helper functions.
 * It's like other pieces in that it has a name, and a layout and a color (which is the color when first instantiated)
 * Boards can be created at will to test out different combinations.
 * THere will always be the main board held by the Game but other Board objects will be created when running algos
 * So a copy method will probably also be needed at some point
 */
class Board(size: Int) extends Piece {
  val name = "Board"
  val color = Ansi.BrightBlack
  val layout: Array[Array[Cell]] = Piece.getBoardLayout(size, color)

  // the board outputs unoccupied cells so just call toString on every piece
  // different than the Piece.toString which will not output unoccupied Cell's in other pieces
  // this method is mapped in from Piece.toString
  override def cellToStringMapFunction(cell:Cell): String = cell.toString

  def placeThreePieces(pieces: List[Piece]):Boolean = pieces.forall(placePiece)

  private def placePiece(piece:Piece):Boolean = {
    val legal = legalPlacements(piece)
    if (legal.isEmpty)
      false
    else {

      // eventually this will fail... - we'll be coding that later.
      val loc = legal.head

      for {r <- piece.layout.indices
        c <- piece.layout(0).indices
        cell = piece.layout(r)(c)
        if cell.occupied
      } {
        val replaceCell = new Cell(cell.occupied, cell.color)
        layout(r + loc._1)(c + loc._2) = replaceCell
      }
      true
    }
  }


  private def unoccupiedCells():List[(Int,Int)] = {

    val a = for {r <- layout.indices;c <-layout(0).indices;if !layout(r)(c).occupied} yield (r,c)
    a.toList

  }

  private def legalPlacements(piece: Piece):List[(Int,Int)] = {
    // walk through each unoccupied position on the board
    // see if the piece fits at that position, if it does, add that position to the list
    for { loc <- unoccupiedCells() if legalPlacement(piece, loc) } yield loc

  }

  private def legalPlacement(piece: Piece, loc: (Int, Int)): Boolean = {

    val locRow = loc._1
    val locCol = loc._2

    if ((piece.rows + locRow) > rows) return false // exceeds the bounds of the board - no point in checking any further
    if ((piece.cols + locCol) > cols) return false // exceeds the bounds of the board - no point in checking any further
    
    // this for comprehension will find all instances
    // where the piece has an occupied value and the board has an unoccupied value
    // to yield the unoccupied status of each corresponding position on the board
    val boardOccupiedVector: Seq[Boolean] = for {
      
      r <- piece.layout.indices
      c <- piece.layout(0).indices
      pieceOccupied = piece.layout(r)(c).occupied
      boardUnoccupied = !layout(r + locRow)(c + locCol).occupied
      if pieceOccupied
      if boardUnoccupied
      
    } yield boardUnoccupied

    // basically, this vector will have a bunch of true values because each possible position on the board is unoccupied
    // being !empty is good enough here
    boardOccupiedVector.nonEmpty
    

  }

}