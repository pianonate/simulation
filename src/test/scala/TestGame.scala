/**
 * Created by nathan on 1/16/17.
 * game tests
 */
import org.scalatest.FlatSpec

import scala.collection.mutable

// todo test that all positions are actually being evaluated in a simulation

trait GameInfoFixture {
  val multiGameStats = MultiGameStats(0, 0, 0, 1, new GameTimer)
  val context = new Context(new Conf(Seq()))
  context.gamesToPlay = 1
  context.show = false

}

// todo - using standard scala json library, validate that both weights and game json are valid
class TestGame extends FlatSpec {

  behavior of "A game"

  it must "simulate all legal positions for all permutations" in {
    new GameInfoFixture {

      // set up a game that will have a row and a column that will clear at the same time
      private val plcList = List(
        PieceLocCleared(GamePieces.h3Line, Loc(0, 0), clearedLines = false),
        PieceLocCleared(GamePieces.h2Line, Loc(0, 5), clearedLines = false),
        PieceLocCleared(GamePieces.h5Line, Loc(5, 9), clearedLines = false)
      )
      // setReplayList will put the game in a mode where it only plays from the specified list (in this case the one above)
      context.setReplayList(plcList)
      context.ignoreSimulation = false

      // this test will use 3 different pieces each time it runs
      // which is just an added boost of randomness to make sure everything is good
      context.stopGameAtRound = 1
      context.simulationSelfTest = true

      context.parallel = false

      private val game = new Game(context, multiGameStats)
      private val gameResults: GameResults = game.run
      private val selfTestResults: SelfTestResults = game.getSelfTestResults

      private val simulatedPositions = selfTestResults.simulatedPositions
      private val linesClearedPositions = selfTestResults.linesClearedPositions

      private val legalPositionsSet = selfTestResults.legalPositions.toSet
      private val simulatedPositionsSet = simulatedPositions.toSet

      private val missingSimulations = legalPositionsSet.diff(simulatedPositionsSet)
      private val missingLegal = simulatedPositionsSet.diff(legalPositionsSet)

      private val expectedSimulationCount = legalPositionsSet.size + linesClearedPositions.size

      assert(missingSimulations.isEmpty, "there are legal positions that are unsimulated")
      assert(missingLegal.isEmpty, "there are simulations that didn't have an associated legal position")
      assert(selfTestResults.simulatedPositions.size===expectedSimulationCount,
        "the simulated positions is different than the expected simulation count (legal positions + cleared lines count)")
    }
  }

  it must "run all simulations for three singletons" in {
    new GameInfoFixture {
      private val plcList = List(
        PieceLocCleared(GamePieces.singleton, Loc(0, 0), clearedLines = false),
        PieceLocCleared(GamePieces.singleton, Loc(0, 0), clearedLines = false),
        PieceLocCleared(GamePieces.singleton, Loc(0, 0), clearedLines = false)
      )
      context.setReplayList(plcList)
      context.stopGameAtRound = 1
      context.ignoreSimulation = false
      private val game = new Game(context, multiGameStats)
      private val results: GameResults = game.run

      private val positions = Board.BOARD_SIZE * Board.BOARD_SIZE
      assert(results.totalSimulations === positions * (positions - 1) * (positions - 2), "expected number of simulations did not happen")
      assert(results.totalUnsimulatedSimulations === 0, "unsimulated count should be zero when all the pieces are the same")

    }
  }

  // todo - set up all line clearing scoring combinations and test them for lines cleared of 1 to 6
  // todo - update brendan doc with line clearing scoring algo Array(10,30,60,100,150,210)

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

      // 50 is a magic value - but this MUST be the score based on the pieces setup above
      assert(results.score === 50)
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
      assert(results.score === 20)
    }
  }

  // weighting scheme doesn't guarantee a particular choice - we're looking to ensure that a second round can be played

  it must "place three big boxes on a particular board that invoked a comparison bug" in {
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

      val board = new Board("testBoard", Board.BOARD_COLOR, 0, grid, colorGrid, context.specification)

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

}
