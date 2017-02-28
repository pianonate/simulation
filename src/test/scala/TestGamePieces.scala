/**
  * Created by nathan on 2/27/17.
  * validating GamePieces behavior
  */
import org.scalatest.{ FlatSpec, _ }

class TestGamePieces extends FlatSpec {


  behavior of "GamePieces"


  it must "return the same set of pieces when seeded with the same value" in {

    val seed = new scala.util.Random().nextInt
    val pieces1 = new GamePieces(seed)
    val pieces2 = new GamePieces(seed)

    // validate 100 pieces - which should give you confidence that you're getting the same pieces based on
    // the current seed
    (0 until 100) foreach { _ =>
      val piece1 = pieces1.getRandomPiece
      val piece2 = pieces2.getRandomPiece
      assert(piece1===piece2, "pieces are not the same - piece1: " + piece1.name + " piece2: " + piece2.name)
    }

  }

}

