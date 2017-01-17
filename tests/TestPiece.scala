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

    pieces foreach {piece =>
      val occupancyLength = piece.grid.occupancyGrid.flatten.filter(_ == true).length
      assert(piece.pointValue===occupancyLength)
    }

  }

}

