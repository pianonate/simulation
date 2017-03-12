/**
 * Created by nathan on 1/11/17.
 * tests for Pieces object
 */
import org.scalatest.{FlatSpec, _}

class TestPieces extends FlatSpec {

  behavior of "the Pieces container"

  it must "contain 19 Piece elements in .pieceList" in {
    val pieces = Context().getGamePieces(nextSeed = true)

    assert(pieces.pieceList.length == 19)
  }

}