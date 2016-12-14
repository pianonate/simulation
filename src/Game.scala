/**
 * Created by nathan on 12/10/16.
 * Game will hold a reference to the current board and will also invoke simulations
 * algo research - measure of entropy is a decent approach:
 *    http://stats.stackexchange.com/questions/17109/measuring-entropy-information-patterns-of-a-2d-binary-matrix
 *
 */
object Game {

  object GameOver extends Exception {}

  private val board = new Board(10)
  private var score = 0
  private var result = (0,0)
  private var rowsCleared = 0
  private var colsCleared = 0

  def run(): Unit = {

    try {
      do {

        // get 3 random pieces
        val pieces = List.fill(3)(Piece.getRandomPiece)
        //val pieces = Piece.getNamedPieces("VerticalLine5","VerticalLine5","VerticalLine5")

        pieces foreach { piece =>
          if (!board.placePiece(piece)) throw GameOver
          piece.usage += 1
          score += piece.pointValue
          val result = board.clearLines()
          rowsCleared += result._1
          colsCleared += result._2
          score += (rowsCleared + colsCleared) * board.layout.length
        }

        showPieces(pieces)

        showBoardFooter()

      } while (Console.in.read != 'q')

    } catch {

      case GameOver => // normal game over
      case e: Throwable => println("abnormal run termination:\n" + e.toString)

    } finally {
      showEndGame()
    }
  }

  private def showEndGame() = {

    println

    Piece.pieces.foreach { piece => println(String.format("%19s", piece.name) + " used: " + piece.usage) }

    println
    println("GAME OVER!!\n\nFinal Score: " + score)
    println("Total Pieces Used: " + Piece.pieces.foldLeft(0)((sum, cell) => sum + cell.usage))
    println("Total Rows Cleared: " + rowsCleared)
    println("Total Cols Cleared: " + colsCleared)


  }

  private def showBoardFooter() = {
    println(board.toString)
    println("Occupied positions: " + board.occupiedCount)
    println("type enter to place another piece and 'q' to quit")
  }

  private def showPieces(pieces: List[Piece]): Unit = {

    // we'll need the height of the tallest piece as a guard for shorter pieces where we need to
    // print out spaces as placeholders.  otherwise array access in the for would be broken
    // if we did some magic to append fill rows to the pieces as strings array...
    val tallestPieceHeight = pieces.map(piece => piece.rows).reduceLeft((a, b) => if (a > b) a else b)

    // because we're not printing out one piece, but three across, we need to split
    // the toString from each piece into an Array.  In this case, we'll create a List[Array[String]]
    val piecesToStrings = pieces map { piece =>

      val a = piece.toString.split('\n')
      if (a.length < tallestPieceHeight)
        a ++ Array.fill(tallestPieceHeight - a.length)(piece.printFillString) // fill out the array
      else
        a // just return the array

    }

    println("Placed these pieces:")

    // turn arrays into a list so you can transpose them
    // transpose will create a list of 1st rows, a list of 2nd rows, etc.
    // then print them out - accross and then newline delimit
    piecesToStrings.map(a => a.toList)
      .transpose
      .foreach { l => print(l.mkString); println }

    // one more newline
    println
    println("Score: " + score)

  }

}