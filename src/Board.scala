/**
 * Created by nathan on 12/9/16.
 * Board is the game Board plus helper functions.
 * It's like other pieces in that it has a name, and a layout and a color (which is the color when first instantiated)
 * Boards can be created at will to test out different combinations.
 * THere will always be the main board held by the Game but other Board objects will be created when running algos
 * So a copy method will probably also be needed at some point
 */
class Board(size: Int, val name: String = "Board") extends Piece {

  val color = Ansi.BrightBlack
  val layout: Array[Array[Cell]] = Piece.getBoardLayout(color, size)

  // the board outputs unoccupied cells so just call toString on every piece
  // different than the Piece.toString which will not output unoccupied Cell's in other pieces
  // this method is mapped in from Piece.toString
  override def cellToStringMapFunction(cell:Cell): String = cell.toString

  def clearLines(): (Int,Int) = {


    // TODO: there has got to be a way to remove this duplication
    def clearRow(line:Array[Cell]):Unit = { for (i <-line.indices) line(i) = new Cell(false,this.color) }
    def clearCol(col:Int):Unit = {for ( i <- layout.indices) layout(i)(col) = new Cell(false,this.color) }

    def fullLine(line: Array[Cell]): Boolean = line.forall(cell=>cell.occupied)

    def fullLines(piece:Piece):Seq[Int] = {
      for {i <- piece.layout.indices
        if fullLine(piece.layout(i))}
        yield i
    }

    val clearable = fullLines(this)


    val rotated = Piece.rotate90("Board", this)
    val rotatedClearable = fullLines(rotated)

    clearable.foreach(i => clearRow(this.layout(i)))
    rotatedClearable.foreach(clearCol)

    (clearable.length,rotatedClearable.length)


  }


  // we call this one Naive because it simply tries to find the best outcome of this piece only
  // we need a more robust solution
  def naivePlacePiece(piece:Piece, loc: Option[(Int,Int)]):Boolean = {

    val legal = legalPlacements(piece)
    if (legal.isEmpty)
      false // tells the game that it is over - no legal pieces left
    else {

      // best location is going to attempt all possible locations on a copy of the board
      // it will call clearLines after it makes the try
      // then it will return the current occupied count as the first of a tuple of occupied count / location pairs
      // sort by the occupied count and return the first (lowest) one
      val bestLocation = legal.map(loc => naiveTryPlacement(piece, loc)).sortBy(_._1)

      // .head will have the lowest occupied count so actually place the pice this time
      place(piece, bestLocation.head._2, this)

      // sentinel telling the game that we safely placed a piece and it's not game over
      true
    }
  }

  def placeKnownLegal(piece:Piece, loc: Option[(Int,Int)]) : Boolean = loc match {

    case Some(loc) => place(piece, loc, this);true
    case None => false

  }


  def simulatePlacement(piece: Piece, loc:(Int,Int)):Board = {

    // place the piece on a copy, clear the lines
    // and return the occupied count and location that results from that occupied count
    // we'll then sort the result of all occupied counts to see which one has the lowest value
    // and from there - we're golden
   // val boardCopy = Board.copy(piece.name + "_board", this)
    place(piece,loc,this)
    this.clearLines()
    this
  }

  //todo: this goes away if the simulation version works
  def naiveTryPlacement(piece: Piece, loc:(Int,Int)):(Int, (Int, Int)) = {

    // place the piece on a copy, clear the lines
    // and return the occupied count and location that results from that occupied count
    // we'll then sort the result of all occupied counts to see which one has the lowest value
    // and from there - we're golden
    val boardCopy = Board.copy(piece.name + "_board", this)
    place(piece,loc,boardCopy)
    boardCopy.clearLines()
    (boardCopy.occupiedCount, loc)

  }



  // todo: this one could go away if we get rid of tryPlacement and all of that - or maybe not
  // instead the Game will be simulating on legal locations so will just use
  // the other definition of place
  def place(piece: Piece, loc:(Int,Int), board: Board):Unit  = {

    // todo: evaluate using a view here
    for {r <- piece.layout.indices
      c <- piece.layout(0).indices
      cell = piece.layout(r)(c)
      if cell.occupied
    } {
      val replaceCell = new Cell(cell.occupied, cell.color, true)
      board.layout(r + loc._1)(c + loc._2) = replaceCell
    }
  }

  private def unoccupiedCells():List[(Int,Int)] = {

    // removed the filter as it didn't work for pieces such as the BigLowerRightEl next to something it could fit around
    // Todo: maybe you can improve the algo but maybe you do have to just evaluate each location.  think about it first - this might be ok
    val a = for {r <- layout.indices;c <-layout(0).indices/*;if !layout(r)(c).occupied*/} yield (r,c)
    a.toList

  }

  def legalPlacements(piece: Piece):List[(Int,Int)] = {
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
    val boardCellUnoccupied: Seq[Boolean] = for {
      
      r <- piece.layout.indices
      c <- piece.layout(0).indices
      pieceOccupied = piece.layout(r)(c).occupied
      boardUnoccupied = !layout(r + locRow)(c + locCol).occupied
      if pieceOccupied
      if boardUnoccupied
      
    } yield boardUnoccupied

    // basically, this vector will have a bunch of true values because each possible position on the board is unoccupied
    boardCellUnoccupied.size==piece.pointValue
    

  }

}

object Board {
  def copy(newName: String, boardToCopy: Board):Board = {

    val newBoard = new Board(boardToCopy.layout.length, newName)

    for {
      i <- boardToCopy.layout.indices
      j<-boardToCopy.layout(i).indices
      cell = boardToCopy.layout(i)(j)
    } {
      newBoard.layout(i)(j) = Cell.copy(cell)
    }

    newBoard
  }
}