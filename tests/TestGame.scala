/**
 * Created by nathan on 1/16/17.
 * game tests
 */
import org.scalatest.FlatSpec

trait GameInfoFixture {
  val gameInfo = GameInfo(0, 0, 0, 1, new GameTimer)
  val context = new Context(new Conf(Seq()))
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
      private val results: GameResults = game.run

      // 39 is a magic value - but this MUST be the score based on the pieces setup above
      assert(results.score === 39)
    }
  }

  // this test currently CAN'T work.  this is because in a multi-threaded, two different game runs
  // may find a best score for a set of pieces that are equivalent but the pieces are in a
  // different position on the board.  A way to deal with this would be to put in a tie-breaker that
  // favors any result that is closer to 0,0.  this tie-breaker - I think - would cause
  // repeated games to be compared and they would result in the same answer
  it must "end up with the same score for two consecutive seeded games in parallel mode" ignore {
    new GameInfoFixture {

      context.randomSeed= (new scala.util.Random()).nextInt(1000000000)
      context.stopGameAtRound=10
      context.show = true
      context.parallel = true

      while (true) {
        val game = new Game(context, gameInfo)
        val results1: GameResults = game.run

        val game2 = new Game(context, gameInfo)
        val results2: GameResults = game2.run

        assert(results1.totalSimulations === results2.totalSimulations, "total simulations should match - unsimulated1 " + results1.totalUnsimulatedSimulations + " unsimulated2 " + results2.totalUnsimulatedSimulations)
        assert(results1.score === results2.score, "scores should match")
        assert(results1.rounds === results2.rounds, "rounds should match")
        println(results1.totalSimulations)
        Thread.sleep(2000)
      }
    }
  }
}
