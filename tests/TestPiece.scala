/**
 * Created by nathan on 1/10/17.
 * using tests to add add a BitVector grid
 */
import org.scalatest.{ FlatSpec, _ }

class TestPiece extends FlatSpec {


  behavior of "A Piece"


  it must "have a grid that matches the colorGrid in size" in {

    // validate all the pieces we create
    val pieces = new Pieces().pieceList

    def check(name: String, testType: String, colorGridValue:Int, gridValue:Int) = {
        assert(colorGridValue==gridValue, testType + " for piece: " + name)
    }


    pieces.foreach {piece =>
      check(piece.name, "rows", piece.colorGrid.length, piece.grid.rows)
      check(piece.name, "cols", piece.colorGrid(0).length, piece.grid.cols)

    }
  }

  it must "have a grid that matches the colorGrid in layout" in {
    val pieces = (new Pieces).pieceList

    for {piece <- pieces
      colorGrid = piece.colorGrid
      i <- colorGrid.indices
      j <- colorGrid(i).indices
    } assert(colorGrid(i)(j).occupied==piece.grid.occupied(i,j), " - " + piece.name + ": grids don't match")
  }

}

