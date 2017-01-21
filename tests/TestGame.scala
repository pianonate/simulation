/**
 * Created by nathan on 1/16/17.
 * game tests
 */
import org.scalatest.FlatSpec

trait GameInfoFixture {
  val gameInfo = GameInfo(0, 0, 1, new GameTimer)
  val context = new Context(Array())
  context.continuousMode = false
  context.show = false
}

class TestGame extends FlatSpec {

  // todo - test that clearing a row and a column at the same time results in the correct score
  //        9 row, 9 col then place a singleton should result in a score of (9 + 9 + 1 + 10 + 9 = 38)

  behavior of "A game"

  it must "result in the correct score after clearing a row and a column simultaneously" in {
    new GameInfoFixture {

      // set up a game that will have a row and a column that will clear at the same time
      private val plcList = List(
        PieceLocCleared(Pieces.h5Line, Loc(0, 0), clearedLines = false), // 5
        PieceLocCleared(Pieces.h4Line, Loc(0, 5), clearedLines = false), // 4 rt:9
        PieceLocCleared(Pieces.v5Line, Loc(5, 9), clearedLines = false), // 5 rt:14
        PieceLocCleared(Pieces.v4Line, Loc(1, 9), clearedLines = false), // 4 rt 18
        PieceLocCleared(Pieces.singleton, Loc(0, 9), clearedLines = true), // 1 rt:19 w+ 10 + 9 :rt 38
        PieceLocCleared(Pieces.singleton, Loc(0, 0), clearedLines = false) // rt:39
      )

      // setReplayList will put the game in a mode where it only plays from the specified list (in this case the one above)
      context.setReplayList(plcList)

      private val game = new Game(context, gameInfo)
      private val results: (Int, Int, Int) = game.run

      // 39 is a magic value - but this MUST be the score based on the pieces setup above
      assert(results._1 === 39)
    }
  }
}
