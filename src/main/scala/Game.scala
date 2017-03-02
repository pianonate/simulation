/**
 * Created by nathan on 12/10/16.
 * Game will hold a reference to the current board and will also invoke simulations
 *
 * todo - hook this up to ifttt maker channel http://www.makeuseof.com/tag/ifttt-connect-anything-maker-channel/
 * todo for Richard Kim - check to see if it's windows and output cls rather than clear
 *
 */

import scala.collection.GenSeq
import scala.sys.process._
import scala.language.postfixOps
import Implicits._

object GameOver extends Exception

case class GameResults(
  score:                       Int,
  rounds:                      Int,
  bestPerSecond:               Int,
  totalSimulations:            Long,
  totalUnsimulatedSimulations: Long,
  gameTimer:                   GameTimer
)

case class SelfTestResults(
  legalPositions:        scala.collection.mutable.ListBuffer[Long],
  simulatedPositions:    scala.collection.mutable.ListBuffer[Long],
  linesClearedPositions: scala.collection.mutable.ListBuffer[Long]

)

// this constructor is used in testing to pass in a pre-constructed board state
class Game(context: Context, multiGameStats: MultiGameStats, board: Board) {

  // this is the normal constructor
  def this(context: Context, multiGameStats: MultiGameStats) {
    // initial board creation just requires a size - initialize with all proper defaults
    this(
      context,
      multiGameStats,
      new Board(Board.BOARD_SIZE, context.specification)
    )
  }

  private[this] val gameStats: GameStats = new GameStats

  private[this] val gameSeed = context.getGameSeed
  private[this] val gamePieces: GamePieces = new GamePieces(gameSeed)

  private[this] val score = Counter()
  private[this] val rowsCleared = Counter()
  private[this] val colsCleared = Counter()
  private[this] val rounds = Counter()
  private[this] val gameTimer = new GameTimer
  private[this] val nonSimulationTimer = new GameTimer()

  private[this] val bullShit = new BullShit(rounds, gameTimer)

  // used in simulationSelfTest mode
  // this was the only way I could figure out how to test the
  // logic that skips simulations correctly
  private[this] val legalPositionsSelfTest = scala.collection.mutable.ListBuffer[Long]()
  private[this] val simulatedPositionsSelfTest = scala.collection.mutable.ListBuffer[Long]()
  private[this] val clearedLinesPositionsSelfTest = scala.collection.mutable.ListBuffer[Long]()

  def getSelfTestResults: SelfTestResults = SelfTestResults(legalPositionsSelfTest, simulatedPositionsSelfTest, clearedLinesPositionsSelfTest)

  // used in testing only
  private var lastRoundJSON:String = ""
  def getLastRoundJSON = lastRoundJSON

  def run: GameResults = {

    /*    def resetTerminalBuffer: Unit = {
      // reset terminal every 1000 rounds
      // if (rounds.value % context.eraseTerminalBufferAfterRound == 0)
      "printf '\u001B[3J'" !
    }*/

    try {

      // set the game number and gameSeed so they are part of the JSON file name
      if (context.logJSON) {
        context.setJSONFileNameDiscriminators(multiGameStats.gameNumber, gameSeed)
        logWeights()
      }

      /* def loop = {
        var b = 0*/
      do {

        // todo figure out how to capture a pause character without having to hit return

        roundHandler()

      } while (true) /*while (b == -2)
        println("char after loop: " + b)

      }


      loop*/

    } catch {

      case GameOver =>
      case e: Throwable =>
        println("abnormal run termination:\n" + e.toString)
        context.gamesToPlay = 0
        throw new IllegalArgumentException
    }

    showGameOver()

    // return the score and the number of rounds to Main - where such things are tracked across game instances
    GameResults(
      score.value,
      rounds.value,
      if (context.replayGame && context.ignoreSimulation) 0 else gameStats.bestPerSecond,
      if (context.replayGame && context.ignoreSimulation) 0l else gameStats.totalSimulations,
      if (context.replayGame && context.ignoreSimulation) 0l else gameStats.totalUnsimulatedSimulations,
      gameTimer
    )

  }

  private def roundHandler() = {

    def getReplayPieces: List[PieceLocCleared] = {

      val instrumentedPlcList: List[PieceLocCleared] =
        if (context.replayGame) {
          val next3 = context.takeReplayPiecesForRound.toList
          if (next3.length < 3)
            gameOver
          else
            next3
        } else {
          List()
        }

      instrumentedPlcList
    }

    def piecesForRound(replayPieces: List[PieceLocCleared]): List[Piece] = {
      if (context.replayGame) replayPieces.map(_.piece) else getPiecesForRound
    }

    def getTheChosenOne(results: List[SimulationInfo], replayPieces: List[PieceLocCleared]): Simulation = {
      if (context.replayGame && context.ignoreSimulation)
        Simulation(replayPieces, this.board, 0)
      else {
        // take the best result form each simulation, sort it and select the top
        // in some rounds, you will get an infeasible solution so be sure to ensure the
        // plcList is nonEmpty
        val filtered = results.map(_.best).filter(_.plcList.nonEmpty)
        if (filtered.isEmpty)
          gameOver
        else
          filtered.sortBy(-_.weightedSum).head
      }
    }

    def getResults(pieces: List[Piece]): List[SimulationInfo] = {
      if (context.replayGame && context.ignoreSimulation) Nil else getSimulationResults(pieces)
    }

    def getSimulationResultsString(results: List[SimulationInfo], bestSimulation: Simulation): String = {
      if (context.replayGame && context.ignoreSimulation)
        "\ninstrumented game - skipping results\n"
      else {
        val resultsString = "\n" + context.specification.getSimulationResultsString(results, bestSimulation, bullShit.iterator.next) + "\n"
        resultsString
      }

    }

    def getChosenPlcList(replayPieces: List[PieceLocCleared], bestSimulation: Simulation): List[PieceLocCleared] = {
      // rather than reverse as they're constructed to put them in the right order
      // just reverse now on the specific best piece
      if (context.replayGame && context.ignoreSimulation) replayPieces else bestSimulation.plcList.reverse
    }

    def placePieces(chosenList: List[PieceLocCleared]): List[PieceHandlerInfo] = {

      // zip the piece location cleared list it's index so we don't have to keep a
      // global track of placed pieces
      chosenList.zipWithIndex.map {
        case (plc, index) =>
          pieceHandler(
            plc.piece,
            plc.loc,
            index + 1
          )
      }

    }

    def getPlacePiecesResultsString(pieceHandlerInfoList: List[PieceHandlerInfo]): String = {

      // zip the piece location cleared list it's index so we don't have to keep a
      // global track of placed pieces
      val a: List[List[String]] = pieceHandlerInfoList.map {
        info => getPieceHandlerResults(info).split("\n")
      }.transpose

      val boards = a.map(each => each.mkString(StringFormats.VERTICAL_LINE + " ".repeat(3))).mkString("\n")
      "\n".repeat(2) + boards

    }

    def getUnplacedPiecesString(bestSimulation: Simulation): String = {

      val s = (bestSimulation.pieceCount until 3).map({ index =>
        "\nDammit!  Couldn't place piece " + (index + 1) + "\n"
      }).mkString("\n")

      s
    }

    def getColorGridBitMask: math.BigInt = {

      var big = math.BigInt(0)

      // only update from 0 if we are logging JSON
      if (context.logJSON) {

        var row = 0
        var col = 0

        while (row < Board.BOARD_SIZE) {
          while (col < Board.BOARD_SIZE) {

            if (this.board.cachedOccupancyGrid(row)(col)) {

              val color = this.board.colorGrid(row)(col)
              val colorInt = gamePieces.colorIndexMap(color)
              val shift = row * (Board.BOARD_SIZE * 4) + col * 4
              val shifted = math.BigInt(colorInt) << shift
              big = big + shifted

            }

            col += 1
          }
          col = 0
          row += 1
        }

      }

      big
    }

    def clearScreen(): Unit = {
      "clear".!
    }

    /*   def moveCursorTopLeft: Unit = {
      //"clear" !
      "tput cup 10 4" !
      // "printf \u001B[2J" !
    }*/

    val replayPieces = getReplayPieces

    // get either three random pieces, or the replayPieces passed in
    val pieces = piecesForRound(replayPieces)

    rounds.inc()

    nonSimulationTimer.pause()

    val results = getResults(pieces)

    nonSimulationTimer.resume()

    val bestSimulation = getTheChosenOne(results, replayPieces)

    if (bestSimulation.pieceCount > 0) {

      val chosenList: List[PieceLocCleared] = getChosenPlcList(replayPieces, bestSimulation)

      // get the bit mask based on the board state before pieces are placed
      // then pass the bitmask along to json logger
      val colorGridBitMask = getColorGridBitMask

      // as a side effect of placing, returns a string representing board states
      // is there a better way to do this?
      val placePiecesInfo = placePieces(chosenList)

      val placePiecesString = getPlacePiecesResultsString(placePiecesInfo)

      if (context.show) {
        clearScreen()

        val endOfRoundResultsString = getRoundResultsString

        if (!context.showRoundResultsOnly) {
          val simulationResultsString = getSimulationResultsString(results, bestSimulation)
          val unplacedPiecesString = getUnplacedPiecesString(bestSimulation)
          print(
            simulationResultsString +
              placePiecesString +
              unplacedPiecesString
          )
        }

        print(endOfRoundResultsString)
      }

      logRound(results, bestSimulation, placePiecesInfo, colorGridBitMask)
    }

    if (bestSimulation.pieceCount < GamePieces.numPiecesInRound || (rounds.value == context.stopGameAtRound)) {
      gameOver
    }

  }

  private def gameOver: Nothing = {
    // flush is a test to see what happens after a long running game ends
    //System.out.flush()
    /* System.err.flush()*/
    // it didn't work.  now do what Keith suggests and add a Thread.sleep(2000) - stupid
    // if this works i'm going to be pissed
    //println("\nSLEEPING FOR 10\n\n\nSLEEPING FOR 10\n\n\nSLEEPING FOR 10!!!")
    //Thread.sleep(10000)
    throw GameOver
  }

  private def getSimulationResults(pieces: List[Piece]): List[SimulationInfo] = {
    // set up a test of running through all orderings of piece placement (permutations)
    // and for each ordering, try all combinations of legal locations
    // the best score as defined by Simulations.compare after trying all legal locations is chosen
    val duration = new GameTimer

    if (context.simulationSelfTest) {
      // the test should only be called for one round but just in case it's not
      // then the test will get the last round of information as we will clear
      // these lists each time...thus:
      this.legalPositionsSelfTest.clear()
      this.simulatedPositionsSelfTest.clear()
      this.clearedLinesPositionsSelfTest.clear()
    }

    val permutations = pieces
      .permutations
      .toList

    val paraperms = {
      if (context.parallel)
        permutations.toArray.par
      else
        permutations
    }

    val result = paraperms
      .zipWithIndex
      .map(pieces => simulatePermutation(pieces._1, pieces._2, paraperms.length))
      .toList

    val elapsedMs = duration.elapsedMillisecondsFloor

    val simulatedCount = result.map(_.simulatedCount).sum
    val unsimulatedCount = result.map(_.unsimulatedCount).sum

    val perSecond = duration.perSecond(simulatedCount + unsimulatedCount)

    gameStats.updateStats(
      PerformanceInfo(
        simulatedCount,
        unsimulatedCount,
        result.map(_.rcChangedCountBest).sum,
        elapsedMs,
        perSecond
      )
    )

    result

  }

  private def getPiecesForRound: List[Piece] = {
    // use this method to return specific pieces under specific circumstances
    // under normal conditions if (false
    // just return a random set of 3 pieces
    val gamePiecesForRound = {
      // List(Pieces.box,Pieces.h3line,Pieces.upperLeftEl)
      List.fill(3)(gamePieces.getRandomPiece)
    }

    gamePiecesForRound
  }

  private def simulatePermutation(pieces: List[Piece], permutationIndex: Int, totalPermutations: Int): SimulationInfo = {

    val simulationDuration = new GameTimer
    val simulationCount = Counter()
    val unsimulatedCount = Counter()
    val rcChangedCountBest = Counter()

    // keep best populated
    // as this is better than sorting at the end as it allows for a parallel sort
    // as the compare is called on each thread while walking through the legalPlacements(piece).par
    // provided by getLegal below
    var best: Option[Simulation] = None

    case class BoardPieceLocCleared(board: Board, plc: PieceLocCleared)

    //return the board copy and the number of lines cleared
    def placeMe(piece: Piece, theBoard: Board, loc: Loc): BoardPieceLocCleared = {
      val boardCopy = Board.copy("simulationBoard", theBoard)
      boardCopy.place(piece, loc, updateColor = false)
      val cleared = boardCopy.clearLines
      val isCleared = (cleared.rows + cleared.cols) > 0

      // return the board with an instance of a PieceLocCleared class
      BoardPieceLocCleared(boardCopy, PieceLocCleared(piece, loc, isCleared))

    }

    def createSimulations(board: Board, pieces: List[Piece], linesClearedAcc: Boolean, plcAcc: List[PieceLocCleared], locPieceHashAcc: Long): Unit = {

      def isUpdatable(locPieceHash: Long, linesCleared: Boolean): Boolean = {

        /* locHash is the sum of all locHashes (accumulated on updatableAccumulator
           this permutation is only responsible for it's proportion of these locHashes
           which will always be repeated on each permutation from any given starting board
           other that this, always score if a line is cleared as this is not guaranteed to happen
           on all permutations
           */
        def mustUpdateForThisPermutation: Boolean = {
          (locPieceHash % totalPermutations) == permutationIndex
        }

        // the final piece of the simulation count self test is to account
        // for cleared lines that force a calculation even on a thread
        // that is not responsible for this permuation
        if (linesCleared && !mustUpdateForThisPermutation) {
          synchronized { clearedLinesPositionsSelfTest += locPieceHash }
        }

        // we have to count this one if it clears lines as this can happen on any permutation
        linesCleared || mustUpdateForThisPermutation
      }

      def updateBest(simulation: Simulation): Unit = {

        def simulationIsBetterThanBest: Boolean = {
          /* val oldWay = simulation < best.get */
          val newWay = best.get.weightedSum < simulation.weightedSum
          /* assert(oldWay==newWay)*/
          newWay

        }

        def safeUpdateBest(): Unit = {
          val bestID = best.get.id
          // contention
          if (simulationIsBetterThanBest)
            // as long as best hasn't changed and we are in a better simulation then update it
            synchronized {
              if (best.get.id == bestID) {

                best = Some(simulation)
              } else {
                // if the other thread put a better one in there, then who cares - don't update it
                // first check if simulation updated on the other thread is better than the current simulation
                //if (simulation < best.get) {
                if (simulationIsBetterThanBest) {

                  rcChangedCountBest.inc()

                  best = Some(simulation)
                }
              }
            }
        }

        best match {
          case Some(_) =>
            safeUpdateBest()

          case None => synchronized {
            best match {
              case Some(_) =>
                safeUpdateBest()
              case None =>
                best = Some(simulation) // first simulation is best
            }
          }
        }
      }

      def updateSimulation(plcList: List[PieceLocCleared], board: Board): Unit = {
        val id = simulationCount.inc()
        val simulation = Simulation(plcList /*.reverse*/ , board, id)
        updateBest(simulation)
      }

      def getLegal(board: Board, piece: Piece): GenSeq[Loc] = {
        if (context.parallel)
          board.legalPlacements(piece).par
        else
          board.legalPlacements(piece)
      }

      val piece = pieces.head

      val paralegal: GenSeq[Loc] = getLegal(board, piece)

      def simulationHandler(loc: Loc) = {
        val result = placeMe(piece, board, loc)
        val boardCopy = result.board
        val plc = result.plc

        // each location has a unique value and the value are distributed such that when added together
        // the values are unique.  this allows us to view a combination of locations as unique
        // and given these combinations will be repeated in each simulation, a simulation is only
        // responsible to be created for it's proportion of the total number of simulations
        // we mod the total to make this work

        // fixed a bug that you need to take piece into account.  multiplying it times the pieces prime number
        // (generated at construction) to generate unique values for piece/loc combos
        val locIndex = loc.row * Board.BOARD_SIZE + loc.col
        val locPieceHash = Board.allLocationHashes(locIndex) * piece.prime + locPieceHashAcc

        val plcList = plc :: plcAcc

        // recurse
        // if we have already cleared lines then propagate that so we don't pay the freight
        // in "isUpdatable" where it's an expensive calculation
        val linesCleared = if (linesClearedAcc) linesClearedAcc else plc.clearedLines

        if (pieces.tail.nonEmpty) {

          createSimulations(boardCopy, pieces.tail, linesCleared, plcList, locPieceHash)

        } else {

          // following is for testing that we are simulating all possible simulations
          // this records legal positions
          // it's okay to keep pumping legalPositions in on all threads
          // as the set will guarantee the right count
          if (context.simulationSelfTest)
            synchronized { this.legalPositionsSelfTest += locPieceHash }

          // only add simulation when we've reached the last legal location on this path
          if (isUpdatable(locPieceHash, linesCleared)) {

            // this completes the test of validating we are simulating all legal positions
            // this records all simulations
            if (context.simulationSelfTest)
              synchronized { this.simulatedPositionsSelfTest += locPieceHash }

            updateSimulation(plcList, boardCopy)
          } else {
            unsimulatedCount.inc()
          }

        }
      }

      if (paralegal.nonEmpty) {
        paralegal.foreach(simulationHandler)
      } else {
        // this method was called so there aren't any legal placements in this path
        // then update a simulation with the pieces that were found to fit
        // this allows showing the final pieces placed on the board at the end of the game
        // this particular unfinished simulation will not be chosen as long as another
        // simulation is available that has finished for all 3 pieces
        updateSimulation(plcAcc, board)
      }
    }

    createSimulations(board, pieces, linesClearedAcc = false, List(), 0)

    def emptySimulation: Simulation = Simulation(List(), this.board, 0)

    // now we know how long this one took - don't need to include the time to show or return it
    val elapsedMs = simulationDuration.elapsedMillisecondsFloor.toInt

    // extracted for readability
    val result = SimulationInfo(
      pieces,
      simulationCount.value,
      unsimulatedCount.value,
      best.getOrElse(emptySimulation), // when is best empty?  when there are no legal positions at all
      rcChangedCountBest.value,
      elapsedMs
    )

    result

  }

  // returned from pieceHandler to drive both output to stdout and to json logger
  case class PieceHandlerInfo(
    piece:          Piece,
    loc:            Loc,
    index:          Int,
    unclearedBoard: Board,
    clearedBoard:   Board,
    score:          Int,
    rowsCleared:    Int,
    colsCleared:    Int
  )

  private def pieceHandler(piece: Piece, loc: Loc, index: Int): PieceHandlerInfo = {

    // todo take the returned information from pieceHandler to construct the information string separately

    // approach is to create a copy of the board after each piece is placed but before lines are cleared and then return this board along with the piece,
    // location, score, and cleared rows and cleared cols after the piece is placed as well as a copy of the board after the lines are cleared (just for the last one)
    // then pass this into a method that is split off from this one that has the sole responsibility of constructing
    // the result string - only if we are doing a show results

    board.place(piece, loc, updateColor = true)

    // increment piece usage
    piece.usage.inc()

    // score me baby
    score.inc(piece.pointValue)

    val unclearedBoard = Board.copy("unclearedBoard", this.board)

    val linesClearedResult = board.clearLines
    val linesCleared = linesClearedResult.rows > 0 || linesClearedResult.cols > 0

    if (linesCleared) {

      rowsCleared.inc(linesClearedResult.rows)
      colsCleared.inc(linesClearedResult.cols)
      // score.inc(linesClearedResult.rows * Board.BOARD_SIZE + linesClearedResult.cols * Board.BOARD_SIZE - linesClearedResult.rows * linesClearedResult.cols)
      val total = linesClearedResult.rows + linesClearedResult.cols

      score.inc(Game.lineClearingScore(total))

    }

    val clearedBoard = Board.copy("clearedBoard", this.board)

    PieceHandlerInfo(
      piece,
      loc,
      index,
      unclearedBoard,
      clearedBoard,
      score.value,
      linesClearedResult.rows,
      linesClearedResult.cols
    )

  }

  private def getPieceHandlerResults(pieceHandlerInfo: PieceHandlerInfo): String = {

    def getShowBoard(board: Board) = {
      board.show(board.cellShowFunction)
    }

    val minScoreLength = 5 // to allow for 4 digits and a comma
    // this is a whole lot of string construction... should it be here? or should we split pieceHandler and boardResultsString into two separate operations
    val scoreLength = pieceHandlerInfo.score.shortLabel.length.max(minScoreLength)
    val valuesWidth = scoreLength.max(minScoreLength)

    val boardSpacer = " ".repeat(2)
    val labelWidth = Specification.maxOptFactorLabelLength

    val scoreLabel = "score".leftAlignedPadded(labelWidth).appendColon + pieceHandlerInfo.score.label(valuesWidth).green + boardSpacer

    val newScore = Array(
      scoreLabel,
      "cleared rows".leftAlignedPadded(labelWidth).appendColon + pieceHandlerInfo.rowsCleared.label(valuesWidth) + boardSpacer,
      "cleared cols".leftAlignedPadded(labelWidth).appendColon + pieceHandlerInfo.colsCleared.label(valuesWidth) + boardSpacer,
      " ".leftAlignedPadded(labelWidth) + " ".repeat("".appendColon.length) + " ".repeat(valuesWidth) + boardSpacer,
      ("opt factors".leftAlignedPadded(labelWidth) + " ".repeat("".appendColon.length) + " ".repeat(valuesWidth)).underline + boardSpacer

    )

    val newBoardResultsString = context.specification.spec.values.zip(pieceHandlerInfo.clearedBoard.boardScore.scores)
      .map {
        case (optFactor, scoreComponent) =>
          optFactor.label.leftAlignedPadded(labelWidth).appendColon + scoreComponent.intValue.label(valuesWidth) + boardSpacer
      }.toArray

    val scoreInfoLength = newScore.length + newBoardResultsString.length
    // whichever is longer score info or board length
    val totalHeight = scoreInfoLength.max(Board.BOARD_SIZE)

    val remainingScoreLines = (scoreInfoLength until totalHeight)
      .map(_ => " ".repeat(labelWidth + valuesWidth + boardSpacer.length + 2)).toArray

    val newResults: Array[String] = newScore ++ newBoardResultsString ++ remainingScoreLines

    // leave this assert here in case you add a new optimization factor that causes the list of factors to exceed
    // the length of the board - in which case you'll need to modify the code to allow for a buffer at the end of the
    // board as well as the current situation which allows for a buffer at the end of the score and optimization factors
    // aka remainingLines
    // assert(newResults.length == Board.BOARD_SIZE, "new results don't equal board size:" + newResults.length)

    val appendToBoard = "\n" + (Board.BOARD_SIZE until totalHeight)
      .map(_ => " ".repeat(Board.BOARD_SIZE * 2 - 1 + boardSpacer.length) + "\n").mkString

    val newBoard = (getShowBoard(pieceHandlerInfo.unclearedBoard) + appendToBoard) splice newResults

    val finalBoard = if (pieceHandlerInfo.index == GamePieces.numPiecesInRound) newBoard splice (getShowBoard(this.board) + appendToBoard).split("\n") else newBoard

    // todo - get rid of piece.cellShowFunction - it's got to work better than this

    val placingLabel: String = "Placing " + pieceHandlerInfo.index.firstSecondThirdLabel + " ".repeat(2)
    val placingBuffer = placingLabel.length
    val pieceWidth = pieceHandlerInfo.piece.cols * 2 - 1

    val pieceArray: Array[String] = pieceHandlerInfo.piece.show(pieceHandlerInfo.piece.cellShowFunction).split("\n").map(each => each + " ".repeat(placingBuffer - pieceWidth - 2))

    val placingHeader = Array(placingLabel) ++ pieceArray

    val remainingPieceLines = (placingHeader.length to GamePieces.tallestPiece).map(_ => " ".repeat(placingBuffer)).toArray
    val atLoc = Array(("at " + pieceHandlerInfo.loc.show).leftAlignedPadded(placingBuffer))

    val placingContent = placingHeader ++ remainingPieceLines ++ atLoc
    val remainingPlacingLines = (placingContent.length until finalBoard.length).map(_ => " ".repeat(placingBuffer)).toArray

    (placingContent ++ remainingPlacingLines).mkString("\n").splice(finalBoard.split("\n"))

  }

  private def getRoundResultsString: String = {

    if (context.replayGame && context.ignoreSimulation)
      // todo - how to handle replay mode?
      "Replay Mode - no simulating.  what do you show here?"
    else {

      // todo: after reaching 1.9MM in 2+ hours, the non simulation time was over 7% (next time record exact number)
      //       game 2 - 611,283 - 4.4% non-simulation time after 49M 49s

      def getGameTimeInfo: Array[String] = {

        val gameElapsedNanoseconds = gameTimer.elapsedNanoseconds.toFloat
        val standardTimingsString = Array(
          "",
          ("game " + multiGameStats.gameNumber.shortLabel + " round " + rounds.shortLabel + " results").header,
          // duration info
          "game elapsed time".label + gameTimer.elapsedLabel,
          "non simulation time".label + nonSimulationTimer.elapsedLabelMs + (nonSimulationTimer.elapsedNanoseconds / gameElapsedNanoseconds).percentLabel
        )

        // optional so we add an empty array when not showing this
        val totalElapsedTimeAcrossGamesString = if (multiGameStats.gameNumber > 1) Array("total elapsed time".label + multiGameStats.totalTime.elapsedLabel) else Array[String]()

        standardTimingsString ++ totalElapsedTimeAcrossGamesString
      }

      def getScoreInfo: Array[String] = {

        // average of the last 100
        val newSessionHighScore = score.value > multiGameStats.sessionHighScore
        val sessionHighScore = if (newSessionHighScore) score.value else multiGameStats.sessionHighScore
        val averageScore = if (multiGameStats.averageScore == 0) score.value else multiGameStats.averageScore
        val newMachineHighScore = score.value > multiGameStats.machineHighScore
        val machineHighScore = if (newMachineHighScore) score.value else multiGameStats.machineHighScore

        val a = Array(
          " ",
          "score".label + score.scoreLabel,
          "average score".label + averageScore.label,
          "session high score".label + (if (newSessionHighScore) sessionHighScore.scoreLabel else sessionHighScore.label),
          "all time high score".label + (if (newMachineHighScore) machineHighScore.scoreLabel else machineHighScore.label),
          " ",
          "rows cleared".label + rowsCleared.label,
          "cols cleared".label + colsCleared.label,
          "lines cleared".label + (rowsCleared.value + colsCleared.value).label
        )

        a

      }

      def getPointsInfo: Array[String] = {

        val roundsPerSecond = rounds.value / gameTimer.elapsedSeconds
        val pointsPerSecond = math.floor(score.value / gameTimer.elapsedSeconds).toInt
        val pointsPerMinute = math.floor(score.value / gameTimer.elapsedMinutes).toInt
        val pointsPerHour = math.floor(score.value / gameTimer.elapsedHours).toInt

        val a = Array(

          // points/second info
          " ",
          "rounds per second".label + roundsPerSecond.label,
          "points per second".label + pointsPerSecond.label,
          "points per minute".label + pointsPerMinute.label,
          "points per hour".label + pointsPerHour.label
        )

        a
      }

      def getSimulationInfo: Array[String] = {

        val lastRoundInfo = gameStats.lastRoundInfo
        val skippedPercent: Float = lastRoundInfo.unsimulatedCount.toFloat / lastRoundInfo.simulatedCount.toFloat

        val averagePerSecond = gameStats.averagePerSecond

        val simulationInfoString = Array(
          // simulation info
          " ",
          "simulations".label + lastRoundInfo.simulatedCount.label + lastRoundInfo.elapsedMs.msLabel(3),
          "skipped simulations".label + lastRoundInfo.unsimulatedCount.label + skippedPercent.skippedPercentLabel,
          // speed info
          " ",
          "simulations/sec".label + lastRoundInfo.perSecond.perSecondLabel,
          "average/sec last 100".label + averagePerSecond.yellowPerSecondLabel,
          "best per second".label + gameStats.bestPerSecond.greenPerSecondLabel,
          // race condition info
          " ",
          "race cond. on best".label + gameStats.totalRaceConditionOnBest.label + " (" + lastRoundInfo.rcChangedCountBest.shortLabel + ")",
          ""

        )

        simulationInfoString

      }

      val gameTimeInfo = getGameTimeInfo
      val scoreInfo = getScoreInfo
      val pointsInfo = getPointsInfo
      val simulationInfo = getSimulationInfo

      (gameTimeInfo ++ scoreInfo ++ pointsInfo ++ simulationInfo).mkString("\n")

    }

  }

  private def showGameOver() = {
    if (context.show) {

      val s = "\n" + "GAME OVER!!".redHeader + "\n" +
        "game info".header + "\n" +
        "final Score".label + score.scoreLabel + "\n" +
        "rounds".label + rounds.label + "\n" +
        "rows cleared".label + rowsCleared.label + "\n" +
        "cols cleared".label + colsCleared.label + "\n" +
        "game elapsed time".label + gameTimer.elapsedLabel + "\n"

      // todo - there is a bug in this because each piece is re-used in gamePieces
      //        unless you get gamePieces to create new pieces, you'll just keep incrementing
      //println(labelFormat.format("piece distribution"))
      //println(gamePieces.usageString)

      print(s)
    }
  }

  val delimiter = ","

  private def getNameVal(name: String, value: Any): String = {
    getNameVal(name, value, delimited = true)
  }

  private def getNameVal(name: String, value: Any, delimited: Boolean) = {
    "\"" + name + "\":" + value.toString + (if (delimited) delimiter else "")
  }

  private def logWeights(): Unit = {

    if (context.logJSON) {
      val weightJSON = context.specification.getWeightsJSON
      context.jsonLogger.info(weightJSON)
    }

  }

  private def logRound(results: List[SimulationInfo],
    best: Simulation,
    pieceHandlerInfo: List[PieceHandlerInfo],
    colorGridBitMask: BigInt): Unit = {

    // log pieces - format is in ./src/main/resources/sample.json
    // only log if asked to do so via command line parameter -j, --logjson

    if (context.logJSON) {

      def getScores(scores: Array[ScoreComponent]): String = {
        scores.map { scoreComponent =>

          (
            "type".jsonNameValuePair("Feature".doubleQuote) +
            "name".jsonNameValuePair(scoreComponent.label.doubleQuote) +
            "intVal".jsonNameValuePair(scoreComponent.intValue) +
            "normalizedVal".jsonNameValuePair(scoreComponent.normalizedValue) +
            "weightedVal".jsonNameValuePairLast(scoreComponent.weightedValue)

          ).curlyBraces

        }.mkString(JSONFormats.delimiter)
      }

      val selectedPieces = pieceHandlerInfo.map { info =>

        (
          "type".jsonNameValuePair("PieceLoc".doubleQuote) +
          "name".jsonNameValuePair(info.piece.name.doubleQuote) +
          "row".jsonNameValuePair(info.loc.row) +
          "col".jsonNameValuePair(info.loc.col) +
          "rowsCleared".jsonNameValuePair(info.rowsCleared) +
          "colsCleared".jsonNameValuePairLast(info.colsCleared)

        ).curlyBraces

      }.mkString(JSONFormats.delimiter)

      val endOfRoundScores = getScores(this.board.boardScore.scores)

      val permutations = results.zipWithIndex.map {
        case (p, i) =>

          val winner = p.best == best

          val pType = "type".jsonNameValuePair("Permutation".doubleQuote)
          val index = "index".jsonNameValuePair(i + 1) // add 1 because permutations are not 0 based in the _real_ world
          val winnerString = "winner".jsonNameValuePair(if (winner) "true" else "false")

          val pieces = p.pieces.reverse.map(piece =>

            (
              "type".jsonNameValuePair("Piece".doubleQuote) +
              "name".jsonNameValuePairLast(piece.name.doubleQuote)
            ).curlyBraces).mkString(JSONFormats.delimiter)

          val pieceArrayString = "pieces".jsonNameValuePair(pieces.squareBracket)

          val scores = getScores(p.best.board.boardScore.scores)
          val permutationScores = "permutationScores".jsonNameValuePairLast(scores.squareBracket)

          (pType + index + winnerString + pieceArrayString + permutationScores).curlyBraces

      }.mkString(JSONFormats.delimiter)

      val s = (
        "type".jsonNameValuePair("Game".doubleQuote) +
        "round".jsonNameValuePair(rounds.value) +
        "score".jsonNameValuePair(score.value) +
        "linesCleared".jsonNameValuePair(rowsCleared.value + colsCleared.value) +
        "gamePieceSeed".jsonNameValuePair(gameSeed) +
        "beginOfRoundColorGridBitMask".jsonNameValuePair(colorGridBitMask) +
        "endOfRoundBitMask".jsonNameValuePair(this.board.grid.asBigInt) +
        "selectedPieces".jsonNameValuePair(selectedPieces.squareBracket) +
        "endOfRoundScores".jsonNameValuePair(endOfRoundScores.squareBracket) +
        "permutations".jsonNameValuePairLast(permutations.squareBracket)
        ).curlyBraces

      this.lastRoundJSON = s

      context.jsonLogger.info(s)
    }

  }

}

object Game {

  // provides the score for clearing 1 to 6 (max possible) lines at once
  private val lineClearingScore: Array[Int] = {
    val r = 1 to 6
    // Array(0,10,30,60,100,150,210)
    r.scanLeft(0)(_ + _ * 10).toArray
  }
}