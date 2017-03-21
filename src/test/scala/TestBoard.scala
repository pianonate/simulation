/**
 * Created by nathan on 1/11/17.
 * tests for Board objects
 */
import org.scalatest.{FlatSpec, _}

trait BoardFixture {

  val context = Context()
  context.show = false

  val specification: Specification = context.specification

  val boardSize: Int = context.boardSize
  val board = new Board(context)
  val gamePieces: GamePieces = context.getGamePieces
  val initialOccupied: Int = board.grid.popCount
  val initialOpenLines: Int = board.grid.openLineCount

  def addRow(at: Int): Unit = {

    board.place(gamePieces.h5Line, Loc(at, 0), updateColor = true)
    board.place(gamePieces.h5Line, Loc(at, 5), updateColor = true)

  }

  def addCol(at: Int): Unit = {
    board.place(gamePieces.v5Line, Loc(0, at), updateColor = true)
    board.place(gamePieces.v5Line, Loc(5, at), updateColor = true)
  }
}

trait BoardCopyFixture extends BoardFixture {

  board.place(gamePieces.getRandomPiece, Loc(boardSize / 2, boardSize / 2), updateColor = true) // just place one in the middle

  val copy: Board = Board.copy(board)

  val sourceColorGrid: Array[Array[String]] = board.colorGrid
  val sourceGrid: OccupancyGrid = board.grid

  val copyColorGrid: Array[Array[String]] = copy.colorGrid
  val copyGrid: OccupancyGrid = copy.grid
}

class TestBoard extends FlatSpec {

  behavior of "A board"

  it must "contribute to code coverage %" in {
    new BoardFixture {
      val simulation = Simulation(Array(PieceLocCleared(gamePieces.bigBox,Loc(0,0),true)), board, 32)
      val s = simulation.toString
      assert(s.toString.length > 0)
    }
  }

  it must "give a valid toString for OccupancyGrid" in {
    val o = OccupancyGrid(10, 10, true, BoardSizeInfo(10))
    val s = o.toString
    assert(s.length > 0)

  }

  it must "generate a correct show result for a board" in {
    new BoardFixture {

      addRow(0)

      val s = board.show
      // executes code in show that
      assert(s.split("\n").length === context.boardSize)
    }
  }

  it must "count 2 neighbors correctly" in {
    new BoardFixture {
      board.place(gamePieces.singleton, Loc(0, 0), updateColor = true)
      val score: Int = board.boardScore.twoNeighborsScore.intValue
      assert(score == 5, "placing a singleton at the upper left should result in a two neighbor count of 5")
    }
  }

  it must "make an exact copy" in {
    new BoardCopyFixture {

      for {
        i <- sourceGrid.occupancyGrid.indices
        j <- sourceGrid.occupancyGrid(i).indices
      } {

        // copy is only for simulations so color grid's are no longer expected to match as we don't copy the color grid for simulations
        //assert(sourceColorGrid(i)(j) == copyColorGrid(i)(j), "- color grids don't match") // ensure source and destination match
        assert(sourceGrid.occupied(i, j) == copyGrid.occupied(i, j), "- BitVector grids don't match") // ensure OccupancyGrid grids also match

      }

    }
  }

  it must "not reflect changes in underlying board when changes are made on a copy" in {
    new BoardCopyFixture {
      // place the random piece at the beginning
      val piece: Piece = gamePieces.getRandomPiece
      board.place(piece, Loc(0, 0), updateColor = true) // we know that the source board is empty at (0,0) as it is not filled in on the fixture

      // iterate through piece indices as they will match the board at location (0,0)
      for {
        i <- piece.grid.occupancyGrid.indices
        j <- piece.grid.occupancyGrid(i).indices
        if piece.grid.occupancyGrid(i)(j)
      } {
        // color grids are not copied because they are only used for simulations
        assert(sourceGrid.occupied(i, j) != copyGrid.occupied(i, j), "- OccupancyGrids shouldn't match")
      }
    }
  }

  it must "reflect the correct score after placing pieces" in {
    new BoardFixture {
      var i = 0
      // note - this is quick and dirty and potentially fragile
      //        as there is no optimization that runs and if the pieces
      //        are returned in a different order, we may not
      //        clear enough lines to have this execute safely
      for (piece <- gamePieces.pieceList) {
        board.clearLines
        val boardScore = board.grid.popCount
        val pieceScore = piece.pointValue
        val loc = board.legalPlacements(piece).head
        board.place(piece, loc, updateColor = true)
        assert(board.grid.popCount === boardScore + pieceScore, "- " + piece.name + " - index:" + i)
        i += 1
      }
    }
  }

  it must "reduce open lines by board size + 1 after adding a row" in {
    new BoardFixture {
      addRow(0)
      assert(board.grid.openLineCount === (initialOpenLines - (boardSize + 1)))
      assert(board.grid.popCount == (initialOccupied + boardSize))
    }
  }

  it must "reduce open lines by board size + 1 after adding a col" in {
    new BoardFixture {
      addCol(0)
      assert(board.grid.openLineCount === (initialOpenLines - (boardSize + 1)))
      assert(board.grid.popCount == (initialOccupied + boardSize))
    }
  }

  it must "have no open lines after adding a row and a column (invalid state)" in {
    new BoardFixture {
      addCol(0)
      addRow(0)
      assert(initialOpenLines - (boardSize * 2) === 0)
      assert(board.grid.popCount === (initialOccupied + boardSize + boardSize - 1))
    }
  }

  it must "have same number of open lines after adding and clearing the same row" in {
    new BoardFixture {

      addRow(boardSize / 2)
      board.clearLines
      assert(board.grid.openLineCount === initialOpenLines)

    }
  }

  it must "have same number of open lines after adding and clearing the same column" in {
    new BoardFixture {
      addCol(boardSize / 2)
      board.clearLines
      assert(board.grid.openLineCount === initialOpenLines)

    }
  }

  it must "have the same number of open lines after adding and clearing a row and a column" in {
    new BoardFixture {
      addCol(boardSize / 2)
      addRow(boardSize / 2)
      board.clearLines
      assert(board.grid.openLineCount === initialOpenLines)

    }
  }

  it must "clear four full rows and four full columns" in {
    new BoardFixture {
      (0 to 3).foreach(i => addRow(i))
      assert(board.grid.popCount === (4 * boardSize))
      assert(board.clearLines.rows === 4)
      assert(board.grid.popCount === 0)

      (0 to 3).foreach(i => addCol(i))
      assert(board.grid.popCount === (4 * boardSize))
      assert(board.clearLines.cols === 4)
      assert(board.grid.popCount === 0)

    }
  }

  it must "find the correct contiguous lines" in {
    new BoardFixture {

      0 until boardSize foreach { i =>
        val expected = { if (i < boardSize / 2) boardSize - (i + 1) else i }

        addRow(i)
        val rowMax = board.grid.maxContiguousOpenLines
        assert(expected === rowMax)
        board.clearLines

        addCol(i)
        val colMax = board.grid.maxContiguousOpenLines
        assert(expected === colMax)
        board.clearLines

      }
      /*
      0 - 9
      1 - 8
      2 - 7
      3 - 6
      4 - 5
      5 - 5
      6 - 6
      7 - 7
      8 - 8
      9 - 9
      */
    }

  }

  it must "have empty occupancy at each grid position after clearing a row on an empty board" in {
    new BoardFixture {
      val occupancy: Int = board.grid.popCount
      addRow(0)
      board.clearLines
      for {
        i <- board.colorGrid.indices
        j <- board.colorGrid(0).indices
      } assert(!board.grid.occupied(i, j))
    }
  }

  it must "count legal placements for a piece correctly" in {
    new BoardFixture {
      def expected(piece: Piece): Int = {
        (boardSize - piece.rows + 1) * (boardSize - piece.cols + 1)
      }
      for {
        piece <- gamePieces.pieceList
        board = new Board(context)
      } {
        val legal = board.legalPlacements(piece).length
        assert(expected(piece) === legal)
      }
    }
  }

}