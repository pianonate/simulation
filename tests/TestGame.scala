/**
 * Created by nathan on 1/16/17.
 * game tests
 */
import org.scalatest.FlatSpec

trait GameInfoFixture {
  val gameInfo = GameInfo(0, 0, 0, new GameTimer)
  val context = new Context(Array())

}

class TestGame extends FlatSpec {

  // todo - test that clearing a row and a column at the same time results in the correct score
  //        9 row, 9 col then place a singleton should result in a score of (9 + 9 + 1 + 10 + 9 = 38)

  behavior of "A game"

  it must "result in the correct score after clearing a row and a column simultaneously" in {
    new GameInfoFixture {

      context.continuousMode = false
      context.show = false

      val plcList = List(
        PieceLocCleared(Pieces.h5line, Loc(0, 0), clearedLines = false), // 5
        PieceLocCleared(Pieces.h4line, Loc(0, 5), clearedLines = false), // 4 rt:9
        PieceLocCleared(Pieces.v5line, Loc(5, 9), clearedLines = false), // 5 rt:14
        PieceLocCleared(Pieces.v4line, Loc(1, 9), clearedLines = false), // 4 rt 18
        PieceLocCleared(Pieces.singleton, Loc(0, 9), clearedLines = true), // 1 rt:19 w+ 10 + 9 :rt 38
        PieceLocCleared(Pieces.singleton, Loc(0, 0), clearedLines = false) // rt:39
      )

      context.setInstrumentedList((plcList))

      val game = new Game(context, gameInfo)
      val results = game.run
      assert(results._1===39)
    }
  }
}
