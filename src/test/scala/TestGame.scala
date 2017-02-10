/**
 * Created by nathan on 1/16/17.
 * game tests
 */
import org.scalatest.FlatSpec

trait GameInfoFixture {
  val multiGameStats = MultiGameStats(0, 0, 0, 1, new GameTimer)
  val context = new Context(new Conf(Seq()))
  context.gamesToPlay = 1
  context.show = false
}

class TestGame extends FlatSpec {

  behavior of "A game"

  it must "result in the correct score after clearing a row and a column simultaneously" in {
    new GameInfoFixture {

      // set up a game that will have a row and a column that will clear at the same time
      private val plcList = List(
        PieceLocCleared(GamePieces.h5Line, Loc(0, 0), clearedLines = false), // 5
        PieceLocCleared(GamePieces.h4Line, Loc(0, 5), clearedLines = false), // 4 rt:9
        PieceLocCleared(GamePieces.v5Line, Loc(5, 9), clearedLines = false), // 5 rt:14
        PieceLocCleared(GamePieces.v4Line, Loc(1, 9), clearedLines = false), // 4 rt 18
        PieceLocCleared(GamePieces.singleton, Loc(0, 9), clearedLines = true), // 1 rt:19 w+ 10 + 9 :rt 38
        PieceLocCleared(GamePieces.singleton, Loc(0, 0), clearedLines = false) // rt:39
      )

      // setReplayList will put the game in a mode where it only plays from the specified list (in this case the one above)
      context.setReplayList(plcList)

      private val game = new Game(context, multiGameStats)
      private val results: GameResults = game.run

      // 39 is a magic value - but this MUST be the score based on the pieces setup above
      assert(results.score === 39)
    }
  }

  it must "not get a weighted score larger than 1 when the board is cleared" in {
    new GameInfoFixture {
      // set up a game that will have a row and a column that will clear at the same time
      private val plcList = List(
        PieceLocCleared(GamePieces.h5Line, Loc(0, 0), clearedLines = false),
        PieceLocCleared(GamePieces.h4Line, Loc(0, 5), clearedLines = false),
        PieceLocCleared(GamePieces.singleton, Loc(5, 9), clearedLines = false)
      )
      // setReplayList will put the game in a mode where it only plays from the specified list (in this case the one above)
      context.setReplayList(plcList)
      context.ignoreSimulation = false
      context.show = false
      private val game = new Game(context, multiGameStats)

      // there is an assertion that will run if any normalized score is larger than 1
      private val results: GameResults = game.run

      // a score of 20 ensures that a line was cleared
      assert(results.score===20)
    }
  }

  // weighting scheme doesn't guarantee a particular choice - we're looking to ensure that a second round can be played

  it must "place three big boxes on a particular board that invoked a comparison bug" ignore {
    new GameInfoFixture {

      // this board was an end of game scenario where sometimes
      // under thread race conditions, the wrong board would be chosen
      // when the plcList length was 2 when there was a 3 option available
      // add an additional else clause in Simulation.compare to account for this
      // the following ensures we don't regress
      val a = Array(
        Array(0, 0, 0, 0, 0, 0, 1, 1, 1, 0), //0
        Array(0, 0, 1, 1, 0, 0, 0, 0, 1, 1), //1
        Array(0, 1, 1, 0, 0, 0, 0, 0, 0, 0), //2
        Array(0, 1, 1, 1, 0, 0, 0, 0, 0, 0), //3
        Array(0, 1, 1, 1, 0, 0, 0, 0, 0, 0), //4
        Array(0, 1, 1, 1, 1, 0, 0, 0, 0, 0), //5
        Array(0, 1, 1, 1, 0, 0, 0, 0, 0, 0), //6
        Array(0, 1, 1, 1, 0, 0, 0, 0, 0, 0), //7
        Array(0, 1, 0, 1, 1, 1, 0, 1, 1, 1), //8
        Array(0, 1, 0, 1, 1, 1, 1, 1, 1, 1) //9
      )

      val grid = OccupancyGrid(Board.BOARD_SIZE, Board.BOARD_SIZE, filled = false)
      val colorGrid: Array[Array[String]] = Array.fill[String](10, 10)(Board.BOARD_COLOR)

      for {
        i <- a.indices
        j <- a(0).indices
      } {
        if (a(i)(j) == 1) {
          grid.occupy(i, j)
          colorGrid(i)(j) = StringFormats.GREEN
        }
      }

      val board = new Board("testBoard", Board.BOARD_COLOR, grid, colorGrid, context.specification)

      val plcList = List(
        PieceLocCleared(GamePieces.bigBox, Loc(0, 0), clearedLines = false),
        PieceLocCleared(GamePieces.bigBox, Loc(0, 5), clearedLines = false),
        PieceLocCleared(GamePieces.bigBox, Loc(5, 9), clearedLines = false),
        PieceLocCleared(GamePieces.singleton, Loc(5, 9), clearedLines = false),
        PieceLocCleared(GamePieces.singleton, Loc(5, 9), clearedLines = false),
        PieceLocCleared(GamePieces.singleton, Loc(5, 9), clearedLines = false)


      )

      context.setReplayList(plcList)
      context.ignoreSimulation = false
      context.show = false

      val game = new Game(context, multiGameStats, board)
      val results: GameResults = game.run
      assert(results.rounds === 2)

    }
  }

  /*// this test currently CAN'T work.  this is because in a multi-threaded, two different game runs
  // may find a best score for a set of pieces that are equivalent but the pieces are in a
  // different position on the board.  A way to deal with this would be to put in a tie-breaker that
  // favors any result that is closer to 0,0.  this tie-breaker - I think - would cause
  // repeated games to be compared and they would result in the same answer
  it must "end up with the same score for two consecutive seeded games in parallel mode" ignore {
    new GameInfoFixture {

      context.randomSeed = new scala.util.Random().nextInt(1000000000)
      context.stopGameAtRound = 10
      context.show = false
      context.parallel = true

      while (true) {
        val game = new Game(context, multiGameStats)
        val results1: GameResults = game.run

        val game2 = new Game(context, multiGameStats)
        val results2: GameResults = game2.run

        assert(results1.totalSimulations === results2.totalSimulations, "total simulations should match - unsimulated1 " + results1.totalUnsimulatedSimulations + " unsimulated2 " + results2.totalUnsimulatedSimulations)
        assert(results1.score === results2.score, "scores should match")
        assert(results1.rounds === results2.rounds, "rounds should match")
        println(results1.totalSimulations)
        Thread.sleep(2000)
      }
    }
  } */

}
