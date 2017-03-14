/**
  * Created by nathan on 2/27/17.
  * validating GamePieces behavior
  */
import org.scalatest.{ FlatSpec, _ }

class TestGamePieces extends FlatSpec {


  behavior of "GamePieces"


  it must "return the same set of pieces when seeded with the same value" in {

    // -m sets a s specific seed
    val seedArg = Array("-" + Conf.gameSeedArg + "1")
    val context = Context(seedArg)

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
}

