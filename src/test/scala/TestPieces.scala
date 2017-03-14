/**
 * Created by nathan on 1/11/17.
 * tests for Pieces object
 */
import org.scalatest.{FlatSpec, _}

class TestPieces extends ContextSpec {

  behavior of "the Pieces container"

  it must "contain 19 Piece elements in .pieceList" in {
    val context = getContext()
    val pieces = context.getGamePieces

    assert(pieces.pieceList.length == 19)
  }

}