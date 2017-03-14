/**
  * Created by nathan on 2/27/17.
  * validating GamePieces behavior
  */

class TestGamePieces extends ContextSpec {


  behavior of "GamePieces"


  it must "return the same set of pieces when seeded with the same value" in {

    val context = getContext(gameSeedArg)

    val pieces1 = context.getGamePieces
    val pieces2 = context.getGamePieces

    // validate 100 pieces - which should give you confidence that you're getting the same pieces based on
    // the current seed
    (0 until 100) foreach { _ =>
      val piece1 = pieces1.getRandomPiece
      val piece2 = pieces2.getRandomPiece
      assert(piece1.name===piece2.name, "pieces are not the same - piece1: " + piece1.name + " piece2: " + piece2.name)
    }

  }

  it must "have 8 distinct colors" in {
    val context = getContext()
    val pieces = context.getGamePieces
    val colors = pieces.pieceList.map(_.color).toSet
    assert(colors.size === 9)
  }
}

