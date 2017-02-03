/**
 * Created by nathan on 12/10/16.
 * Game will hold a reference to the current board and will also invoke simulations
 *
 * todo show the recent n games in the round summary
 *
 * Todo: at end of game you can often place one or two pieces, but we don't place any - find out why
 *
 * TODO: Kevin suggests assigning values from 0 to 5 to 0 on both axes and minimize for placement on higher valued squares
 *       I.e., stay out of the middle
 *
 * Todo: save every move in a game so you can replay it if it's awesome
 *
 * Todo: start looking at the points your game loses at - find out where you disagree with the choices it made
 *
 * Todo: introduce random seed and then follow that particular seeded game through to completion -
 *       you can optimize one game play at a time rather than trying to run them over and over
 *
 * Todo:  save datasets of all of the top say 90% of boards for each of your stats in games where you lose.  then keep track of whether or not you lose in the next
 *        round for each of them.  then you'll have a data set to run a machine learning algorithm of of our stats to better pick the best options
 */

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
  private[this] val emptyResults = BoardScore.getResultArray(context.specification.length)

  private[this] val score = Counter()
  private[this] val rowsCleared = Counter()
  private[this] val colsCleared = Counter()
  private[this] val rounds = Counter()
  private[this] val gameTimer = new GameTimer
  private[this] val nonSimulationTimer = new GameTimer()

  private[this] val bullShit = new BullShit(rounds, gameTimer)

  def run: GameResults = {

    try {

      /* def loop = {
        var b = 0*/
      do {

        // todo figure out how to capture a pause character without having to hit return

        roundHandler()

        /* b = RawConsoleInput.read(false)
          println("char in loop: " + b)*/

      } while (true) /*while (b == -2)
        println("char after loop: " + b)

      }


      loop*/

    } catch {

      case GameOver =>
      case e: Throwable =>
        println("abnormal run termination:\n" + e.toString)
        // todo: find out what type of error assert is throwing and match it
        //       currently assert is used at least in pieceHandler to ensure occupied counts are
        //       not messed up
        throw new IllegalStateException()

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
        Simulation(replayPieces, this.board, context.specification.length, 0, this.emptyResults)
      else {
        // take the best result form each simulation, sort it and select the top
        // in some rounds, you will get an infeasible solution so be sure to ensure the
        // plcList is nonEmpty
        val filtered = results.map(_.best).filter(_.plcList.nonEmpty)
        if (filtered.isEmpty)
          gameOver
        else
          filtered.sorted.head
      }
    }

    def getResults(pieces: List[Piece]): List[SimulationInfo] = {
      if (context.replayGame && context.ignoreSimulation) Nil else getSimulationResults(pieces)
    }

    def getSimulationResultsString(results: List[SimulationInfo], bestSimulation: Simulation): String = {
      if (context.replayGame && context.ignoreSimulation)
        "\ninstrumented game - skipping results\n"
      else {
        val resultsString = "\n" + context.specification.getSimulationResultsString(results, bestSimulation, context.showWorst) + "\n"
        val heightBuffer = if (results.length < 6) (results.length until 6).map(i => i + 1).mkString(":\n") + "\n" else ""
        resultsString + heightBuffer
      }

    }

    def getImprovedSimulationResultsString(results: List[SimulationInfo], bestSimulation: Simulation): String = {
      if (context.replayGame && context.ignoreSimulation)
        "\ninstrumented game - skipping results\n"
      else {
        val resultsString = "\n" + context.specification.getImprovedSimulationResultsString(results, bestSimulation, context.showWorst) + "\n"
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
      val a: List[List[String]] = chosenList.zipWithIndex.map(
        plc => pieceHandler(
          plc._1.piece,
          plc._1.loc,
          plc._2 + 1
        ).split("\n")
      ).transpose

      val boards = a.init.map(each => each.mkString("  ")).mkString("\n")
      val scores = a.last.mkString("\n")

      boards + "\n\n" + scores

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

    /*def resetTerminalBuffer: Unit = {
      // reset terminal every 1000 rounds
      if (rounds.value % context.eraseTerminalBufferAfterRound == 0)
        "printf '\u001B[3J'" !
    }*/

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

      // no need to get a results string if we're not going to show it
      //val simulationResultsString = getSimulationResultsString(results, bestSimulation)

      val improvedSimulationResultsString = getImprovedSimulationResultsString(results, bestSimulation)
      clearScreen()
      print(improvedSimulationResultsString)

      val chosenList: List[PieceLocCleared] = getChosenPlcList(replayPieces, bestSimulation)

      // as a side effect of placing, returns a string representing board states
      // is there a better way to do this?
      // todo - put the board scores next to each board and spread across the screen
      // fewer lines of output
      val placePiecesString = placePieces(chosenList)
      print(placePiecesString)

      val endOfRoundResultsString = getRoundResultsString(multiGameStats.gameCount: Int)
      print(endOfRoundResultsString)

      val unplacedPiecesString = getUnplacedPiecesString(bestSimulation)

      if (context.show) {

        //moveCursorTopLeft
        //clearScreen()

        print(
            //simulationResultsString +
            //endOfRoundResultsString +
            //placePiecesString +
            unplacedPiecesString +
             // improvedSimulationResultsString +
            "\n"
        )

//Thread.sleep(250)
        //Console.out.flush()

        // todo, see if this actually matters with iTerm - which seems to be superior terminal manager
        // resetTerminalBuffer

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
          val simulation = Simulation(plcList /*.reverse*/ , board, context.specification.length, id, this.emptyResults)
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

    def emptySimulation: Simulation = Simulation(List(), this.board, context.specification.length, 0, this.emptyResults)

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

  private def pieceHandler(piece: Piece, loc: Loc, index: Int): String = {

    val boardBuffer = Board.BOARD_SIZE * 2 - 1
    val pieceBuffer = boardBuffer - (piece.cols * 2 - 1)

    def getLinesClearedString(result: ClearedLines): String = {

      val clearedString = if (result.rows > 0 || result.cols > 0) {

        def getLineClearedString(i: Int, s: String): String = {
          val clearedString = if (i > 0) "cleared " + i + " " + s.plural(i) else ""
          val spaces = " " * (boardBuffer - clearedString.length)
          clearedString + spaces + "  \n"
        }

        val r = getLineClearedString(result.rows, "row")
        val c = getLineClearedString(result.cols, "column")

        r + c

      } else
        " " * boardBuffer + "  \n" +
          " " * boardBuffer + "  \n"

      clearedString
    }

    // todo - get rid of piece.cellShowFunction - it's got to work better than this

    val placingString = "\nPlacing " + index.firstSecondThirdLabel + " at " + loc.show + " \n" +
      piece.show(piece.cellShowFunction).split("\n").map(each => each + " " * pieceBuffer).mkString("\n") +
      (piece.rows to GamePieces.tallestPiece).map(_ => "\n" + " " * (boardBuffer + 2)).mkString

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

    val scoreString = "score: " + score.boardScoreLabel + " - results:" + context.specification.getBoardResultString(board.results)
    val s = placingString + "\n" + boardBeforeClearing + "\n" + getLinesClearedString(linesClearedResult) + scoreString

    s
  }

  private def getShowBoard = {
    board.show(board.cellShowFunction) /*.split("\n").zip(board.boardNeighbors).map(s => s._1 + s._2.mkString(" ")).mkString("\n")*/
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
          "all time high score".label + (if (newMachineHighScore) machineHighScore.scoreLabel else machineHighScore.label)
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
          "",
          bullShit.iterator.next,
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

object Game {

  def showGameStart(specification: Specification): Unit = {

    val begin =
      """GAME START
        |
        |This game works by first selecting 3 pieces from the set of all possible pieces.
        |Then it will try all possible orderings (permutations) of placements of those 3 pieces
        |(up to six permutations will be selected depending on whether there are any duplicates).
        |If no lines are cleared then only the first permutation will be evaluated as piece order
        |won't matter so no reason in simulating any other orderings.
        |
        |Then for each permutation, it will try each of the 3 pieces in order on all possible
        |locations on the board, checking to see whether any lines are cleared between each piece placement.
        |Each placement of three pieces along with the board state after placing the third piece is
        |called a Simulation.
        |
        |Each of the following will be evaluated on the final board state:""".stripMargin

    val end = """
        |
        |Taking all of these factors into account, in the order listed above, the best simulation will be chosen.  The
        |ordering of the factors will indicate the optimization strategy.  The factors
        |are output at the end of each simulation run for a permutation - in the order in which they
        |are considered.
        |
        |The best simulation is then used to place pieces and continue onto the next round.""".stripMargin

    val explanations = specification.getOptimizationFactorExplanations

    println(
      begin + "\n" + explanations + end
    )

  }

}
