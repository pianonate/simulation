/**
 * Created by nathan on 1/10/17.
 * using tests to add add a BitVector grid
 */
import org.scalatest.{ FlatSpec, _ }

class TestPiece extends FlatSpec {


  behavior of "A Piece"


  it must "have a grid that matches the colorGrid in size" in {

    // validate all the pieces we create
    val pieces = new GamePieces().pieceList

    pieces foreach {piece =>
      val occupancyLength = piece.grid.occupancyGrid.flatten.count(_ == true)
      assert(piece.pointValue===occupancyLength)
    }

  }

}

