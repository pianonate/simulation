/**
 * Created by nathan on 1/16/17.
 * game tests
 */
import org.scalatest.{Assertion, FlatSpec}

import scala.util.parsing.json.JSON

trait GameInfoFixture {
  val multiGameStats = MultiGameStats(0, 0, 0, 1, new GameTimer)
  val context = Context()

  val specification = context.specification

  val gamePieces = context.getGamePieces(nextSeed = true)
  val boardSize = context.boardSize
  context.gamesToPlay = 1
  context.show = false

}

class TestGame extends FlatSpec {

  behavior of "A game"

  it must "generate valid json weights for logging" in {
    new GameInfoFixture {

      val json = context.specification.getWeightsJSON
      val result = JSON.parseFull(json)

      // you get some map value back if it parsed, otherwise it's going to fail so make sure it doesn't say fail
      assert(result.getOrElse("fail") != "fail")

    }
  }

  it must "generate valid end of round results json for logging" in {
    new GameInfoFixture {

      context.stopGameAtRound = 1
      context.logJSON = true
      private val game = new Game(context, multiGameStats)
      game.run
      // this seems hackish - but it's only for testing so we'll live with it
      private val json = game.getLastRoundJSON
      private val result = JSON.parseFull(json)

      // you get some map value back if it parsed, otherwise it's going to fail so make sure it doesn't say fail
      assert(result.getOrElse("fail") != "fail")

    }
  }

  it must "score all combinations of cleared lines correctly" in {
    new GameInfoFixture {

      def runAndAssert(plc: List[PieceLocCleared], expectedScore: Int): Assertion = {

        context.setReplayList(plc)
        context.ignoreSimulation = true

        val game = new Game(context, multiGameStats)
        val results = game.run
        assert(results.score === expectedScore, "20 is expected from clearing one line")

      }

      private val plcList1 = List(
        PieceLocCleared(gamePieces.h4Line, Loc(0, 0), clearedLines = false),
        PieceLocCleared(gamePieces.h5Line, Loc(0, 4), clearedLines = false),
        PieceLocCleared(gamePieces.singleton, Loc(0, 9), clearedLines = false)
      )

      // 10 points for 1
      runAndAssert(plcList1, 20)

      private val plcList2 = List(
        PieceLocCleared(gamePieces.h4Line, Loc(0, 0), clearedLines = false),
        PieceLocCleared(gamePieces.h5Line, Loc(0, 4), clearedLines = false),
        PieceLocCleared(gamePieces.h4Line, Loc(1, 0), clearedLines = false),
        PieceLocCleared(gamePieces.h5Line, Loc(1, 4), clearedLines = false),
        PieceLocCleared(gamePieces.singleton, Loc(2, 9), clearedLines = false),
        PieceLocCleared(gamePieces.v2Line, Loc(0, 9), clearedLines = false)
      )

      // 30 points for 2
      runAndAssert(plcList2, 51)

      private val plcList3 = List(
        PieceLocCleared(gamePieces.h4Line, Loc(0, 0), clearedLines = false),
        PieceLocCleared(gamePieces.h5Line, Loc(0, 4), clearedLines = false),
        PieceLocCleared(gamePieces.h4Line, Loc(1, 0), clearedLines = false),
        PieceLocCleared(gamePieces.h5Line, Loc(1, 4), clearedLines = false),
        PieceLocCleared(gamePieces.h4Line, Loc(2, 0), clearedLines = false),
        PieceLocCleared(gamePieces.h5Line, Loc(2, 4), clearedLines = false),
        PieceLocCleared(gamePieces.v3Line, Loc(0, 9), clearedLines = false),
        PieceLocCleared(gamePieces.singleton, Loc(9, 9), clearedLines = false),
        PieceLocCleared(gamePieces.singleton, Loc(9, 8), clearedLines = false)

      )

      // 60 points for 3
      runAndAssert(plcList3, 92)

      private val plcList4 = List(
        PieceLocCleared(gamePieces.h4Line, Loc(0, 0), clearedLines = false),
        PieceLocCleared(gamePieces.h5Line, Loc(0, 4), clearedLines = false),
        PieceLocCleared(gamePieces.h4Line, Loc(1, 0), clearedLines = false),
        PieceLocCleared(gamePieces.h5Line, Loc(1, 4), clearedLines = false),
        PieceLocCleared(gamePieces.h4Line, Loc(2, 0), clearedLines = false),
        PieceLocCleared(gamePieces.h5Line, Loc(2, 4), clearedLines = false),
        PieceLocCleared(gamePieces.h4Line, Loc(3, 0), clearedLines = false),
        PieceLocCleared(gamePieces.h5Line, Loc(3, 4), clearedLines = false),
        PieceLocCleared(gamePieces.v4Line, Loc(0, 9), clearedLines = false),
        PieceLocCleared(gamePieces.singleton, Loc(9, 9), clearedLines = false),
        PieceLocCleared(gamePieces.singleton, Loc(9, 8), clearedLines = false),
        PieceLocCleared(gamePieces.singleton, Loc(9, 7), clearedLines = false)
      )

      // 100 points for 4
      runAndAssert(plcList4, 143)

      private val plcList5 = List(
        PieceLocCleared(gamePieces.h4Line, Loc(0, 0), clearedLines = false),
        PieceLocCleared(gamePieces.h5Line, Loc(0, 4), clearedLines = false),
        PieceLocCleared(gamePieces.h4Line, Loc(1, 0), clearedLines = false),
        PieceLocCleared(gamePieces.h5Line, Loc(1, 4), clearedLines = false),
        PieceLocCleared(gamePieces.h4Line, Loc(2, 0), clearedLines = false),
        PieceLocCleared(gamePieces.h5Line, Loc(2, 4), clearedLines = false),
        PieceLocCleared(gamePieces.h4Line, Loc(3, 0), clearedLines = false),
        PieceLocCleared(gamePieces.h5Line, Loc(3, 4), clearedLines = false),
        PieceLocCleared(gamePieces.h4Line, Loc(4, 0), clearedLines = false),
        PieceLocCleared(gamePieces.h5Line, Loc(4, 4), clearedLines = false),
        PieceLocCleared(gamePieces.v5Line, Loc(0, 9), clearedLines = false),
        PieceLocCleared(gamePieces.singleton, Loc(9, 9), clearedLines = false)
      )

      // 150 points for 5
      runAndAssert(plcList5, 201)

      private val plcList6 = List(
        PieceLocCleared(gamePieces.h4Line, Loc(0, 0), clearedLines = false),
        PieceLocCleared(gamePieces.h3Line, Loc(0, 4), clearedLines = false),
        PieceLocCleared(gamePieces.h4Line, Loc(1, 0), clearedLines = false),

        PieceLocCleared(gamePieces.h3Line, Loc(1, 4), clearedLines = false),
        PieceLocCleared(gamePieces.h4Line, Loc(2, 0), clearedLines = false),
        PieceLocCleared(gamePieces.h3Line, Loc(2, 4), clearedLines = false),

        PieceLocCleared(gamePieces.v3Line, Loc(3, 7), clearedLines = false),
        PieceLocCleared(gamePieces.v4Line, Loc(6, 7), clearedLines = false),
        PieceLocCleared(gamePieces.v3Line, Loc(3, 8), clearedLines = false),

        PieceLocCleared(gamePieces.v4Line, Loc(6, 8), clearedLines = false),
        PieceLocCleared(gamePieces.v3Line, Loc(3, 9), clearedLines = false),
        PieceLocCleared(gamePieces.v4Line, Loc(6, 9), clearedLines = false),

        PieceLocCleared(gamePieces.bigBox, Loc(0, 7), clearedLines = false),
        PieceLocCleared(gamePieces.singleton, Loc(9, 0), clearedLines = false),
        PieceLocCleared(gamePieces.singleton, Loc(9, 1), clearedLines = false)

      )

      // 210 points for 6
      runAndAssert(plcList6, 263)

      // 7 is impossible

    }
  }

  private def runSelfTest(context: Context, multiGameStats: MultiGameStats): Unit = {
    // context.show = true
    context.stopGameAtRound = 1
    context.simulationSelfTest = true

    val game = new Game(context, multiGameStats)
    val results = game.run

    val selfTestResults: SelfTestResults = game.getSelfTestResults

    val simulatedPositions = selfTestResults.simulatedPositions
    val linesClearedPositions = selfTestResults.linesClearedPositions

    val legalPositionsSet = selfTestResults.legalPositions.toSet
    val simulatedPositionsSet = simulatedPositions.toSet

    val missingSimulations = legalPositionsSet.diff(simulatedPositionsSet)
    val missingLegal = simulatedPositionsSet.diff(legalPositionsSet)

    // in the new mechanism to only test valid combinations on each thread, we account for line clearing
    // implicitly so we don't need to calculate the line clears Positions size
    val expectedSimulationCount = legalPositionsSet.size // + linesClearedPositions.size

    // if (selfTestResults.simulatedPositions.size != expectedSimulationCount) {
    // selfTestResults.pieces.foreach(piece => println(piece.name))
    // }

    assert(missingSimulations.size === 0, "there are legal positions that are unsimulated")
    assert(missingLegal.size === 0, "there are simulations that didn't have an associated legal position")
    assert(
      simulatedPositionsSet.size === expectedSimulationCount,
      "the simulated positions is different than the expected simulation count (legal positions + cleared lines count)"
    )
  }

  it must "simulate all legal positions when pieces overlap" in {
    new GameInfoFixture {

      // this one was tricky to find - it happens
      // when a piece such as upperLeftEl can fit "inside" the whole of another piece
      // such as bigLowerRightEl.
      // added an offset to pieces to indicate what is the first occupied position
      // which is then used by the mustUpdateForThisPermutation method
      private val overlappingPieces = List(
        PieceLocCleared(gamePieces.upperLeftEl, Loc(0, 0), clearedLines = false),
        PieceLocCleared(gamePieces.v4Line, Loc(0, 0), clearedLines = false),
        PieceLocCleared(gamePieces.bigLowerRightEl, Loc(0, 0), clearedLines = false)
      )

      context.setReplayList(overlappingPieces)
      context.ignoreSimulation = false
      runSelfTest(context, multiGameStats)
    }
  }

  it must "simulate all legal positions when pieces clear lines" in {
    new GameInfoFixture {

      // make sure line clearing still generates the correct count of simulations
      private val lineClearingPieces = List(
        PieceLocCleared(gamePieces.h5Line, Loc(0, 0), clearedLines = false),
        PieceLocCleared(gamePieces.h5Line, Loc(0, 0), clearedLines = false),
        PieceLocCleared(gamePieces.h5Line, Loc(0, 0), clearedLines = false)
      )

      context.setReplayList(lineClearingPieces)
      context.ignoreSimulation = false
      runSelfTest(context, multiGameStats)
    }
  }

  it must "simulate all legal positions for all permutations" in {
    new GameInfoFixture {

      // this one is left in so that random pieces will be chosen
      // each time tests are run
      // and the self-test still functions
      runSelfTest(context, multiGameStats)

    }
  }

  // todo  currently this is setup to use the new "possible combinations algo not the old locPieceHash algo
  // either algo will benefit from replacing List[PieceLocCleared] with Array[PieceLocCleared] but the latter
  // has the advantage of also allowing use of the combinations in the mustUpdateForThisPermutation routine
  // which eliminates duplicate calculations
  // so... first try all of that (replacing List with Array) and for now, ignore this test
  it must "run all simulations for three singletons" in {
    new GameInfoFixture {
      private val plcList = List(
        PieceLocCleared(gamePieces.singleton, Loc(0, 0), clearedLines = false),
        PieceLocCleared(gamePieces.singleton, Loc(0, 0), clearedLines = false),
        PieceLocCleared(gamePieces.singleton, Loc(0, 0), clearedLines = false)
      )
      context.setReplayList(plcList)
      context.ignoreSimulation = false

      context.stopGameAtRound = 1

      private val game = new Game(context, multiGameStats)
      private val results: GameResults = game.run

      private val positions = boardSize * boardSize
      assert(results.totalSimulations === positions * (positions - 1) * (positions - 2), "expected number of simulations did not happen")

      // actual combinations
      private val actualCombinations = (0 until context.boardPositions).combinations(3).toArray.length
      private val expectedUnsimulated = results.totalSimulations - actualCombinations

      assert(results.totalUnsimulatedSimulations === expectedUnsimulated, "unsimulated count should be zero when all the pieces are the same")

    }
  }

  it must "result in the correct score after clearing a row and a column simultaneously" in {
    new GameInfoFixture {

      // set up a game that will have a row and a column that will clear at the same time
      private val plcList = List(
        PieceLocCleared(gamePieces.h5Line, Loc(0, 0), clearedLines = false), // 5
        PieceLocCleared(gamePieces.h4Line, Loc(0, 5), clearedLines = false), // 4 rt:9
        PieceLocCleared(gamePieces.v5Line, Loc(5, 9), clearedLines = false), // 5 rt:14
        PieceLocCleared(gamePieces.v4Line, Loc(1, 9), clearedLines = false), // 4 rt 18
        PieceLocCleared(gamePieces.singleton, Loc(0, 9), clearedLines = true), // 1 rt:19 w+ 10 + 9 :rt 38
        PieceLocCleared(gamePieces.singleton, Loc(0, 0), clearedLines = false) // rt:39
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
        PieceLocCleared(gamePieces.h5Line, Loc(0, 0), clearedLines = false),
        PieceLocCleared(gamePieces.h4Line, Loc(0, 5), clearedLines = false),
        PieceLocCleared(gamePieces.singleton, Loc(5, 9), clearedLines = false)
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

      val grid = OccupancyGrid(boardSize, boardSize, filled = false, context.boardSizeInfo)
      val colorGrid: Array[Array[String]] = Array.fill[String](boardSize, boardSize)(Board.BOARD_COLOR)

      for {
        i <- a.indices
        j <- a(0).indices
      } {
        if (a(i)(j) == 1) {
          grid.occupy(i, j)
          colorGrid(i)(j) = StringFormats.GREEN
        }
      }

      val board = new Board("testBoard", Board.BOARD_COLOR, 0, grid, colorGrid, context.specification, context)

      val plcList = List(
        PieceLocCleared(gamePieces.bigBox, Loc(0, 0), clearedLines = false),
        PieceLocCleared(gamePieces.bigBox, Loc(0, 5), clearedLines = false),
        PieceLocCleared(gamePieces.bigBox, Loc(5, 9), clearedLines = false),
        PieceLocCleared(gamePieces.singleton, Loc(5, 9), clearedLines = false),
        PieceLocCleared(gamePieces.singleton, Loc(5, 9), clearedLines = false),
        PieceLocCleared(gamePieces.singleton, Loc(5, 9), clearedLines = false)

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
