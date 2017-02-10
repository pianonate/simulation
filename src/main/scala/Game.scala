/**
 * Created by nathan on 12/10/16.
 * Game will hold a reference to the current board and will also invoke simulations
 * todo - hook thi sup to ifttt maker channel http://www.makeuseof.com/tag/ifttt-connect-anything-maker-channel/
 * TODO!!! - kevin suggestion - determine actual weights by running each opt factor one thousand times and printing out the average score
 * Todo!!! - change maximizer to short circuit maxmizer that counts how many 3x3's can fit on the board - which maxes out at 3 as you can't get more than 3!!!!
 * Todo!!! - kevin says avoid middle is really important to try
 * Todo for Richard Kim - check to see if it's windows and output cls rather than clear
 *
 */

//import java.util.concurrent.ThreadPoolExecutor.DiscardPolicy
//import java.util.concurrent.{Executors, LinkedBlockingQueue, ThreadPoolExecutor, TimeUnit}

import scala.collection.GenSeq
import scala.sys.process._
import scala.language.postfixOps
import Implicits._

object GameOver extends Exception

case class GameResults(score: Int, rounds: Int, bestPerSecond: Int, totalSimulations: Long, totalUnsimulatedSimulations: Long)

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
  private[this] val gamePieces: GamePieces = new GamePieces(context.randomSeed)

  private[this] val score = Counter()
  private[this] val rowsCleared = Counter()
  private[this] val colsCleared = Counter()
  private[this] val rounds = Counter()
  private[this] val gameTimer = new GameTimer
  private[this] val nonSimulationTimer = new GameTimer()

  private[this] val bullShit = new BullShit(rounds, gameTimer)

  def run: GameResults = {

/*    def resetTerminalBuffer: Unit = {
      // reset terminal every 1000 rounds
      // if (rounds.value % context.eraseTerminalBufferAfterRound == 0)
      "printf '\u001B[3J'" !
    }*/

    try {


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
      if (context.replayGame) 0 else gameStats.bestPerSecond,
      if (context.replayGame) 0l else gameStats.totalSimulations,
      if (context.replayGame) 0l else gameStats.totalUnsimulatedSimulations
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
        val resultsString = "\n" + context.specification.getSimulationResultsString(results, bestSimulation, context.showWorst, bullShit.iterator.next) + "\n"
        resultsString
      }

    }

    def getChosenPlcList(replayPieces: List[PieceLocCleared], bestSimulation: Simulation): List[PieceLocCleared] = {
      // rather than reverse as they're constructed to put them in the right order
      // just reverse now on the specific best piece
      if (context.replayGame && context.ignoreSimulation) replayPieces else bestSimulation.plcList.reverse
    }

    def placePieces(chosenList: List[PieceLocCleared]): String = {
      // zip the piece location cleared list it's index so we don't have to keep a
      // global track of placed pieces
      val a: List[List[String]] = chosenList.zipWithIndex.map {
        case (plc, index) =>
          pieceHandler(
            plc.piece,
            plc.loc,
            index + 1
          ).split("\n")
      }.transpose

      val boards = a.map(each => each.mkString(StringFormats.VERTICAL_LINE + " ".repeat(3))).mkString("\n")
      "\n".repeat(2) + boards

    }

    def getUnplacedPiecesString(bestSimulation: Simulation): String = {
      // todo - store the unplaced piece and print it out

      val s = (bestSimulation.pieceCount until 3).map({ index =>
        //val piece = bestSimulation.plcList(index).piece
        // val pieceString = piece.show(piece.cellShowFunction)
        "\nDammit!  Couldn't place piece " + (index + 1) + "\n" // + pieceString
      }).mkString("\n")

      s
    }

    def clearScreen(): Unit = {
      "clear".!
    }

    def moveCursorTopLeft: Unit = {
      //"clear" !
      "tput cup 10 4" !
      // "printf \u001B[2J" !
    }

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

      // as a side effect of placing, returns a string representing board states
      // is there a better way to do this?
      val placePiecesString = placePieces(chosenList)

      if (context.show) {
        clearScreen()

        val endOfRoundResultsString = getRoundResultsString(multiGameStats.gameCount: Int)

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
    }

    if (bestSimulation.pieceCount < 3 || (rounds.value == context.stopGameAtRound)) {
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

    val rcChangedCountWorst = if (context.showWorst) result.map(_.rcChangedCountWorst).sum else 0

    gameStats.updateStats(
      PerformanceInfo(
        simulatedCount + unsimulatedCount,
        unsimulatedCount,
        result.map(_.rcChangedCountBest).sum,
        rcChangedCountWorst,
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
    val rcChangedCountWorst = Counter()

    // i can't see how to not have this Array
    // i can accumulate locations because you generate one each time through the recursion
    // but you only store the last simulation - so there's nothing to accumulate...
    // val simulations = new Array[Simulation](maxSimulations)

    // keep these populated
    // as this is better than sorting at the end as it allows for a parallel sort
    // as the compare is called on each thread while walking through the legalPlacements(piece).par
    // provided by getLegal below
    var best: Option[Simulation] = None
    var worst: Option[Simulation] = None

    case class BoardPieceLocCleared(board: Board, plc: PieceLocCleared)

    //return the board copy and the number of lines cleared
    def placeMe(piece: Piece, theBoard: Board, loc: Loc): BoardPieceLocCleared = {
      val boardCopy = Board.copy("simulationBoard", theBoard)
      boardCopy.place(piece, loc, updateColor = false)
      val cleared = boardCopy.clearLines(clearColor = false)
      val isCleared = (cleared.rows + cleared.cols) > 0

      // return the board with an instance of a PieceLocCleared class
      BoardPieceLocCleared(boardCopy, PieceLocCleared(piece, loc, isCleared))

    }

    def createSimulations(board: Board, pieces: List[Piece], linesCleared: Boolean, plcAccumulator: List[PieceLocCleared]): Unit = {

      def isUpdatable(loc: Loc): Boolean = {

        // mode the index with totalPermutations and see if it's equal to this permutations index
        // if it is, you must calculate
        def mustUpdateForThisPermutation: Boolean = {
          // find where this location is a flattened index list
          val index = loc.row * Board.BOARD_SIZE + loc.col
          (index % totalPermutations) == permutationIndex
        }

        // we have to count this one if it clears lines as this can happen on any permutation
        linesCleared || mustUpdateForThisPermutation
      }

      def updateBestAndWorst(simulation: Simulation): Unit = {

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

        if (context.showWorst) {

          def simulationIsWorseThanWorst: Boolean = {
            worst.get.weightedSum > simulation.weightedSum
          }

          def safeUpdateWorst(): Unit = {
            val worstID = worst.get.id
            // contention
            if (simulationIsWorseThanWorst)
              // as long as best hasn't changed and we are in a better simulation then update it
              synchronized {
                if (worst.get.id == worstID)
                  worst = Some(simulation)
                else {
                  // if the other thread put a worse one in there, then who cares - don't update it
                  // then check if the new simulation is worse than the new one
                  //if (simulation > worst.get) {
                  if (simulationIsWorseThanWorst) {

                    rcChangedCountWorst.inc()
                    worst = Some(simulation)
                  }
                }
              }
          }

          worst match {
            case Some(_) =>
              safeUpdateWorst()

            case None => synchronized {
              worst match {
                case Some(_) =>
                  safeUpdateWorst()
                case None =>
                  worst = Some(simulation) // first simulation is worst
              }
            }
          }
        }

      }

      def updateSimulation(plcList: List[PieceLocCleared], board: Board): Unit = {
        if (simulationCount.value < context.maxSimulations) {
          val id = simulationCount.inc()
          val simulation = Simulation(plcList /*.reverse*/ , board, id)
          updateBestAndWorst(simulation)
        }
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

        // you can configure max simulations at command line (or in tests)
        // if you want to step through code with fewer simulations executed
        // or if you want to profile with fewer simulations
        if (simulationCount.value < context.maxSimulations) {

          val result = placeMe(piece, board, loc)
          val boardCopy = result.board
          val plc = result.plc

          if (pieces.tail.nonEmpty) {

            // recurse
            // if we have already cleared lines then propagate that so we don't pay the freight
            // in "isUpdatable" where it's an expensive calculation
            val cleared = if (linesCleared) linesCleared else plc.clearedLines
            createSimulations(boardCopy, pieces.tail, cleared, plc :: plcAccumulator)

          } else {
            // right now we never get to the third simulation if it can't fit
            // so it would be good to return a 2 piece simulation or 1 piece simulation if they can be returned
            val plcList = plc :: plcAccumulator

            // only add simulation when we've reached the last legal location on this path
            if (isUpdatable(loc)) {
              updateSimulation(plcList, boardCopy)
            } else {
              unsimulatedCount.inc()
            }
          }
        }
      }

      if (paralegal.nonEmpty) {
        paralegal.foreach(simulationHandler)
      } else {
        // this method was called so the expectation is that there are legal placements
        // given that there are not any legal placements in this path,
        // then update a simulation with, the smaller number of pieces that were found to fit
        // this allows showing the final pieces placed on the board at the end of the game
        // as long as one of the permutations has the ability to place 3 pieces,
        // the following permutation will never be selected to be placed
        // this is guaranteed by Simulation.compare method which considers
        // smaller piece counts to be not comparable
        updateSimulation(plcAccumulator, board)
      }

    }

    createSimulations(board, pieces, linesCleared = false, List())

    def emptySimulation: Simulation = Simulation(List(), this.board, 0)

    // now we know how long this one took - don't need to include the time to show or return it
    val elapsedMs = simulationDuration.elapsedMillisecondsFloor.toInt

    // extracted for readability
    val result = SimulationInfo(
      pieces,
      simulationCount.value,
      unsimulatedCount.value,
      // todo - you could change this to an Option but then all subsequent code would need to change
      best.getOrElse(emptySimulation), // when is best empty?  when there are no legal positions at all
      worst, // it's now an option so it will be None if showSimulation is false
      rcChangedCountBest.value,
      rcChangedCountWorst.value,
      elapsedMs
    )

    result

  }

  // todo take the returned information from pieceHandler to construct the information string separately

  private def pieceHandler(piece: Piece, loc: Loc, index: Int): String = {

    board.place(piece, loc, updateColor = true)

    // increment piece usage
    piece.usage.inc()

    // score me baby
    score.inc(piece.pointValue)

    val boardBeforeClearing = getShowBoard

    val linesClearedResult = board.clearLines(clearColor = true)
    val linesCleared = linesClearedResult.rows > 0 || linesClearedResult.cols > 0

    if (linesCleared) {

      rowsCleared.inc(linesClearedResult.rows)
      colsCleared.inc(linesClearedResult.cols)
      score.inc(linesClearedResult.rows * Board.BOARD_SIZE + linesClearedResult.cols * Board.BOARD_SIZE - linesClearedResult.rows * linesClearedResult.cols)

    }

    val minScoreLength = 5 // to allow for 4 digits and a comma
    // this is a whole lot of string construction... should it be here? or should we split pieceHandler and boardResultsString into two separate operations
    val scoreLength = score.shortLabel.length.max(minScoreLength)
    val valuesWidth = scoreLength.max(minScoreLength)

    val boardSpacer = " ".repeat(2)
    val labelWidth = Specification.maxOptFactorLabelLength

    val scoreLabel = "score".leftAlignedPadded(labelWidth).addColon + score.label(valuesWidth).green + boardSpacer

    val newScore = Array(
      scoreLabel,
      "cleared rows".leftAlignedPadded(labelWidth).addColon + linesClearedResult.rows.label(valuesWidth) + boardSpacer,
      "cleared cols".leftAlignedPadded(labelWidth).addColon + linesClearedResult.cols.label(valuesWidth) + boardSpacer,
      " ".leftAlignedPadded(labelWidth) + " ".repeat("".addColon.length) + " ".repeat(valuesWidth) + boardSpacer,
      ("opt factors".leftAlignedPadded(labelWidth) + " ".repeat("".addColon.length) + " ".repeat(valuesWidth)).underline + boardSpacer

    )

    val newBoardResultsString = context.specification.spec.values.zip(board.boardScore.scores)
      .map {
        case (optFactor, scoreComponent) =>
          optFactor.label.leftAlignedPadded(labelWidth).addColon + scoreComponent.intValue.label(valuesWidth) + boardSpacer
      }.toArray

    val scoreInfoLength = newScore.length + newBoardResultsString.length
    // whichever is longer scoreinfo or board length
    val totalHeight = scoreInfoLength.max(Board.BOARD_SIZE)

    val remainingScoreLines = (scoreInfoLength until totalHeight)
      .map(_ => " ".repeat(labelWidth + valuesWidth + boardSpacer.length + 2)).toArray

    val newResults: Array[String] = newScore ++ newBoardResultsString ++ remainingScoreLines

    // leave this assert here in case you add a new optiization factor that causes the list of factors to exceed
    // the length of the board - in which case you'll need to modify the code to allow for a buffer at the end of the
    // board as well as the current situation which allows for a buffer at the end of the score and optimization factors
    // aka remainingLines
    // assert(newResults.length == Board.BOARD_SIZE, "new results don't equal board size:" + newResults.length)

    val appendToBoard = "\n" + (Board.BOARD_SIZE until totalHeight)
      .map(_ => " ".repeat(Board.BOARD_SIZE * 2 - 1 + boardSpacer.length) +"\n").mkString

    val newBoard = (boardBeforeClearing + appendToBoard) splice newResults

    val finalBoard = if (index == GamePieces.numPiecesInRound) newBoard splice (getShowBoard + appendToBoard).split("\n") else newBoard

    // todo - get rid of piece.cellShowFunction - it's got to work better than this

    val placingLabel: String = "Placing " + index.firstSecondThirdLabel + " ".repeat(2)
    val placingBuffer = placingLabel.length
    val pieceWidth = piece.cols * 2 - 1

    val pieceArray: Array[String] = piece.show(piece.cellShowFunction).split("\n").map(each => each + " ".repeat(placingBuffer - pieceWidth - 2))

    val placingHeader = Array(placingLabel) ++ pieceArray

    val remainingPieceLines = (placingHeader.length to GamePieces.tallestPiece).map(_ => " ".repeat(placingBuffer)).toArray
    val atLoc = Array(("at " + loc.show).leftAlignedPadded(placingBuffer))

    val placingContent = placingHeader ++ remainingPieceLines ++ atLoc
    val remainingPlacingLines = (placingContent.length until finalBoard.length).map(_ => " ".repeat(placingBuffer)).toArray

    (placingContent ++ remainingPlacingLines).mkString("\n").splice(finalBoard.split("\n"))

  }

  private def getShowBoard = {
    board.show(board.cellShowFunction)
  }

  private def getRoundResultsString(gameCount: Int): String = {

    if (context.replayGame && context.ignoreSimulation)
      // todo
      "Replay Mode - no simulating.  what do you show here?"
    else {

      // todo: after reaching 1.9MM in 2+ hours, the non simulation time was over 7% (next time record exact number)
      //       game 2 - 611,283 - 4.4% non-simulation time after 49M 49s

      def getGameTimeInfo: Array[String] = {

        val gameElapsedNanoseconds = gameTimer.elapsedNanoseconds.toFloat
        val standardTimingsString = Array(
          "",
          ("game " + gameCount.shortLabel + " round " + rounds.shortLabel + " results").header,
          // duration info
          "game elapsed time".label + gameTimer.elapsedLabel,
          "non simulation time".label + nonSimulationTimer.elapsedLabelMs + (nonSimulationTimer.elapsedNanoseconds / gameElapsedNanoseconds).percentLabel
        )

        // optional so we add an empty array when not showing this
        val totalElapsedTimeAcrossGamesString = if (multiGameStats.gameCount > 1) Array("total elapsed time".label + multiGameStats.totalTime.elapsedLabel) else Array[String]()

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

        // optional so we add an empty array when not showing this
        val raceConditionOnWorstString = if (context.showWorst) Array("race cond. on worst".label + gameStats.totalRaceConditionOnWorst.label + " (" + lastRoundInfo.rcChangedCountWorst.shortLabel + ")") else Array[String]()

        simulationInfoString ++ raceConditionOnWorstString

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
}



