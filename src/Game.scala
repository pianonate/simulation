/**
 * Created by nathan on 12/10/16.
 * Game will hold a reference to the current board and will also invoke simulations
 * algo research - measure of entropy is a decent approach:
 *    http://stats.stackexchange.com/questions/17109/measuring-entropy-information-patterns-of-a-2d-binary-matrix
 *
 */
object Game {

  private val board = new Board(10)

  def run: Unit = {

    do {

      val pieces = List.fill(3){Piece.randomPiece}
      // val pieces = List(Piece.pieces(7), Piece.pieces(7), Piece.pieces(2))

      showPieces(pieces)

      board.placeThreePieces(pieces)

      println(board.toString)
      println("Occupied positions: " + board.occupiedCount)
      println("type enter to place another piece and 'q' to quit")

    }  while (Console.in.read != 'q')

  }

  private def showPieces(pieces:List[Piece]):Unit = {

    def max(a:Array[String], b:Array[String]):Array[String] = if (a.length > b.length) a else b

    val piecesAsStringArrays = pieces.map(piece => piece.toString.split('\n'))
    val tallestPieceHeight= piecesAsStringArrays.reduceLeft(max).length

    println("Placed these pieces:")

    // i is used to track whether it's possible print a row from a piece
    // or if you have to output a filler string
    for (i <- 0 to tallestPieceHeight) {
      for (piece <-piecesAsStringArrays) {
        if (i < piece.length)
          print(piece(i))
        else {
          // number of chars is the number of boxes (minus  the escape characters)
          // split on box char to get the length
          val fillLength = piece(0).count(_ == Cell.BOX_CHAR.charAt(0)) + piece(0).count(_ == ' ')
          List.fill(fillLength)(" ") foreach print
        }
      }
      print("\n")
    }

  }


}