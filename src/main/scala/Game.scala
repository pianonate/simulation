/**
 * Created by nathan on 12/10/16.
 * Game will hold a reference to the current board and will also invoke simulations
 *
 * todo - hook this up to ifttt maker channel http://www.makeuseof.com/tag/ifttt-connect-anything-maker-channel/
 * todo for Richard Kim - check to see if it's windows and output cls rather than clear
 * todo - specify log file name on command line so that you can have a separate log file for running in the IDE
 * todo - abridged vs. detailed logging - for outputting to brendan vs .ux
 * todo - allowed combinations optimization could work even with cleared lines - use the specified combinations but if you clear lines, have a 2 combo legal and if you clear another line, then all positions are legal
 * todo - create a blog series
 * todo - automatically output the weights from a highscore game to a file that can be copied over the fixed weight map - code generation
 * todo - Conf json log folder
 * todo - Conf simulation.log path/file
 */

import scala.collection.GenSeq
import scala.language.postfixOps
import Implicits._

object GameOver extends Exception

case class GameResults(
  score:                       Int,
  rounds:                      Int,
  bestPerSecond:               Int,
  totalSimulations:            Long,
  totalUnsimulatedSimulations: Long,
  gameTimer:                   GameTimer,
  gameSeed:                    Int
)

case class SelfTestResults(
  legalPositions:     scala.collection.mutable.ListBuffer[Long],
  simulatedPositions: scala.collection.mutable.ListBuffer[Long],
  pieces:             List[Piece]
)

// this constructor is used in testing to pass in a pre-constructed board state
class Game(context: Context, multiGameStats: MultiGameStats, board: Board) extends Output {

  // this is the normal constructor
  def this(context: Context, multiGameStats: MultiGameStats) {
    // initial board creation just requires a size - initialize with all proper defaults
    this(
      context,
      multiGameStats,
      new Board(context)
    )
  }

  private[this] val boardSize: Int = context.boardSize
  private[this] val gameStats: GameStats = new GameStats

  private[this] val gamePieces: GamePieces = context.getGamePieces
  private[this] val gameSeed = gamePieces.seed

  // note when you getGamePieces for a game it always gets the next in the series of random seeds
  // (unless overriden by gameSeed or sessionSeed)
  // in either case, constructing the specificaiton, ti uses the same seed as was used to make the gamePieces
  // this way if you ever want to replay a game, then the the spec will have the same weights on replay
  // need to reset to new random weights each time
  if (!context.fixedWeights)
    context.specification = Specification(random = true, context.getConstructionInfo)

  private[this] val score = Counter()
  private[this] val rowsCleared = Counter()
  private[this] val colsCleared = Counter()
  private[this] val rounds = Counter()
  private[this] val gameTimer = new GameTimer
  private[this] val nonSimulationTimer = new GameTimer

  private[this] val postTimer = new GameTimer
  postTimer.pause()

  private[this] val timerGetPlacePiecesResultsString = new GameTimer
  private[this] val timerGetRoundResultsString = new GameTimer
  private[this] val timerSimulationResultsString = new GameTimer
  private[this] val timerPrintHeader = new GameTimer
  private[this] val timerPrintRoundResults = new GameTimer

  timerGetPlacePiecesResultsString.pause()
  timerGetRoundResultsString.pause()
  timerSimulationResultsString.pause()
  timerPrintHeader.pause()
  timerPrintRoundResults.pause()

  private[this] val bullShit = new BullShit(rounds, gameTimer)

  // used in simulationSelfTest mode
  // this was the only way I could figure out how to test the
  // logic that skips simulations correctly
  private[this] val legalPositionsSelfTest = scala.collection.mutable.ListBuffer[Long]()
  private[this] val simulatedPositionsSelfTest = scala.collection.mutable.ListBuffer[Long]()
  private[this] var piecesSelfTest: Array[Piece] = Array()

  private[this] var selfDestruct = false
  def initiateSelfDestruct(): Unit = selfDestruct = true

  def getSelfTestResults: SelfTestResults = SelfTestResults(
    legalPositionsSelfTest,
    simulatedPositionsSelfTest,
    piecesSelfTest.toList
  )

  // used in testing only
  private var lastRoundJSON: String = ""
  def getLastRoundJSON: String = lastRoundJSON

  def run: GameResults = {

    try {

      // set the game number and gameSeed so they are part of the JSON file name
      if (context.logJSON) {
        context.setJSONFileNameDiscriminators(multiGameStats.gameNumber, gameSeed)
        logWeights()
      }

      do {

        // todo figure out how to capture a pause character without having to hit return
        roundHandler()

        if (this.selfDestruct)
          throw new IllegalStateException("ensure unknown errors are handled")

      } while (true)

    } catch {

      case GameOver =>
      case e: Throwable =>
        println("abnormal run termination:\n" + e.toString)
        throw e
    }

    showGameOver()

    // return the score and the number of rounds to Main - where such things are tracked across game instances
    GameResults(
      score.value,
      rounds.value,
      if (context.replayGame && context.ignoreSimulation) 0 else gameStats.bestPerSecond,
      if (context.replayGame && context.ignoreSimulation) 0l else gameStats.totalSimulations,
      if (context.replayGame && context.ignoreSimulation) 0l else gameStats.totalUnsimulatedSimulations,
      gameTimer,
      gameSeed
    )

  }

  private def roundHandler() = {

    def getReplayPieces: Array[PieceLocCleared] = {

      val instrumentedPlcArray: Array[PieceLocCleared] =
        if (context.replayGame) {
          val next3 = context.getNextReplayPiecesForRound
          if (next3.length < 3)
            gameOver
          else
            next3
        } else {
          Array()
        }

      instrumentedPlcArray
    }

    def piecesForRound(replayPieces: Array[PieceLocCleared]): Array[Piece] = {
      if (context.replayGame) replayPieces.map(_.piece) else getPiecesForRound
    }

    def getTheChosenOne(results: List[SimulationInfo], replayPieces: Array[PieceLocCleared]): Simulation = {
      if (context.replayGame && context.ignoreSimulation)
        Simulation(replayPieces, this.board, 0)
      else {
        // take the best result form each simulation, sort it and select the top
        // in some rounds, you will get an infeasible solution so be sure to ensure the
        // plcArray is nonEmpty
        val filtered = results.map(_.best).filter(_.plcArray.nonEmpty)
        if (filtered.isEmpty)
          gameOver
        else
          filtered.minBy(-_.weightedSum)
      }
    }

    def getResults(pieces: Array[Piece]): List[SimulationInfo] = {
      if (context.replayGame && context.ignoreSimulation) Nil else getSimulationResults(pieces)
    }

    def getSimulationResultsString(results: List[SimulationInfo], bestSimulation: Simulation): String = {
      if (context.replayGame && context.ignoreSimulation)
        "\ninstrumented game - skipping results\n"
      else {
        val resultsString = "\n" + context.specification.getSimulationResultsString(results, bestSimulation, bullShit.iterator.next, gamePieces) + "\n"
        resultsString
      }

    }

    def getChosenPlcArray(replayPieces: Array[PieceLocCleared], bestSimulation: Simulation): Array[PieceLocCleared] = {
      // rather than reverse as they're constructed to put them in the right order
      // just reverse now on the specific best piece
      if (context.replayGame && context.ignoreSimulation) replayPieces else bestSimulation.plcArray.reverse
    }

    def placePieces(chosenList: Array[PieceLocCleared]): List[PieceHandlerInfo] = {

      // reset roundScore for this round
      this.board.roundScore = 0

      // zip the piece location cleared list it's index so we don't have to keep a
      // global track of placed pieces
      chosenList.zipWithIndex.map {
        case (plc, index) =>
          pieceHandler(
            plc.piece,
            plc.loc,
            index + 1
          )
      }.toList
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

      val s = if (bestSimulation.pieceCount < 3) {
        (bestSimulation.pieceCount until 3).map({ index =>
          "\nDammit!  Couldn't place piece " + (index + 1) + "\n"
        }).mkString("\n")
      } else
        ""
      s
    }

    def getColorGridBitMask: math.BigInt = {

      var big = math.BigInt(0)

      // only update from 0 if we are logging JSON
      if (context.logJSON) {

        var row = 0
        var col = 0

        while (row < boardSize) {
          while (col < boardSize) {

            if (this.board.cachedOccupancyGrid(row)(col)) {

              val color = this.board.colorGrid(row)(col)
              val colorInt = gamePieces.colorIndexMap(color)
              val shift = row * (boardSize * 4) + col * 4
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

    val replayPieces = getReplayPieces

    // get either three random pieces, or the replayPieces passed in
    val pieces = piecesForRound(replayPieces)

    rounds.inc()

    nonSimulationTimer.pause()

    val results = getResults(pieces)

    nonSimulationTimer.resume()

    postTimer.resume()

    val bestSimulation = getTheChosenOne(results, replayPieces)

    if (bestSimulation.pieceCount > 0) {

      val chosenList: Array[PieceLocCleared] = getChosenPlcArray(replayPieces, bestSimulation)

      // get the bit mask based on the board state before pieces are placed
      // then pass the bitmask along to json logger
      val colorGridBitMask = getColorGridBitMask

      // as a side effect of placing, returns a string representing board states
      // is there a better way to do this?
      val placePiecesInfo = placePieces(chosenList)

      if (context.show) {

        if (!context.showRoundResultsOnly) {

          timerGetPlacePiecesResultsString.resume()
          val placePiecesString = getPlacePiecesResultsString(placePiecesInfo)
          timerGetPlacePiecesResultsString.pause()

          timerSimulationResultsString.resume()
          val simulationResultsString = getSimulationResultsString(results, bestSimulation)
          timerSimulationResultsString.pause()

          val unplacedPiecesString = getUnplacedPiecesString(bestSimulation)

          timerPrintHeader.resume()
          val sb = new StringBuilder
          sb ++= StringFormats.CLEAR_SCREEN
          sb ++= simulationResultsString
          sb ++= placePiecesString
          sb ++= unplacedPiecesString

          print(sb.toString)

          timerPrintHeader.pause()
        }
        timerPrintRoundResults.resume()

        if (context.showRoundResultsOnly)
          print(StringFormats.CLEAR_SCREEN)

        print(getRoundResultsString)
        timerPrintRoundResults.pause()

      }

      logRound(results, bestSimulation, placePiecesInfo, colorGridBitMask)
    }

    if (bestSimulation.pieceCount < Game.numPiecesInRound || (rounds.value == context.stopGameAtRound)) {
      gameOver
    }

    postTimer.pause()

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

  private def getSimulationResults(pieces: Array[Piece]): List[SimulationInfo] = {

    if (context.simulationSelfTest)
      this.piecesSelfTest = pieces

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

  private def getPiecesForRound: Array[Piece] = {
    // use this method to return specific pieces under specific circumstances
    // under normal conditions if (false
    // just return a random set of 3 pieces
    val gamePiecesForRound = {
      // List(Pieces.box,Pieces.h3line,Pieces.upperLeftEl)
      Array.fill(3)(gamePieces.getRandomPiece)
    }

    gamePiecesForRound
  }

  private def simulatePermutation(pieces: Array[Piece], permutationIndex: Int, totalPermutations: Int): SimulationInfo = {

    val simulationDuration = new GameTimer
    val simulationCount = Counter()
    val unsimulatedCount = Counter()
    val rcChangedCountBest = Counter()

    // keep best populated
    // as this is better than sorting at the end as it allows for a parallel sort
    // as the compare is called on each thread while walking through the legalPlacements(piece).par
    // provided by getLegal below
    var best: Option[Simulation] = None

    case class BoardScorePlc(board: Board, score: Int, plc: PieceLocCleared)

    //return the board copy and the number of lines cleared
    def placeMe(piece: Piece, theBoard: Board, loc: Loc): BoardScorePlc = {

      val boardCopy = Board.copy(theBoard)
      boardCopy.place(piece, loc, updateColor = false)
      val cleared = boardCopy.clearLines
      val totalCleared = cleared.rows + cleared.cols
      val isCleared = totalCleared > 0

      val score = piece.pointValue + context.lineClearingScore(totalCleared)

      // return the board with an instance of a PieceLocCleared class
      BoardScorePlc(boardCopy, score, PieceLocCleared(piece, loc, isCleared))

    }

    def getLocPieceHash(loc: Loc, piece: Piece, locPieceHashAcc: Long) = {
      if (context.simulationSelfTest) {
        // locPieceHash is only used for selfTest
        // each location has a unique value and the value are distributed such that when added together
        // the values are unique.  this allows us to view a combination of locations as unique
        // and given these combinations will be repeated in each simulation, a simulation is only
        // responsible to be created for it's proportion of the total number of simulations
        // we use this unique value to count legal positions and simulated positions in the self test

        val locIndex = loc.row * boardSize + loc.col
        // fixed a bug that you need to take piece into account.  multiplying it times the pieces prime number
        // (generated at construction) to generate unique values for piece/loc combos
        context.allLocationHashes(locIndex) * piece.prime + locPieceHashAcc
      } else 0l
    }

    def createSimulations(board: Board, pieces: Array[Piece], linesClearedAcc: Boolean, plcArrayAcc: Array[PieceLocCleared], scoreAcc: Int, locPieceHashAcc: Long, depth:Int): Unit = {

      def isUpdatable(plcArray: Array[PieceLocCleared], linesCleared: Boolean): Boolean = {
        // improved from (Mac Pro Profiler)
        // 19,000/s - when using toArray
        // 49,500/s - removed toArray
        // 54,182/s - removed lazy val call on offSetToFirstOnPosition - possibly this is just a profiler illusion
        //            but that's okay as the code is now simpler because Board no longer extends Piece
        // 466,743/s - turned List[PieceLocCleared] into Array[PieceLocCleared].  Yeah.

        def mustUpdateForThisPermutation: Boolean = {

          // originally this used locPieceHash method but this is better 
          // as this one will also eliminate duplicates when there are duplicate or triplicate pieces
          // which happens often
          // to make this work, stop using List[PieceLocCleared] and start using an Array[PieceLocCleared]
          // then indexing and getting length and all of that BS will go faster.

          val plc1 = plcArray(0)
          val plc2 = plcArray(1)
          val plc3 = plcArray(2)

          // whoah - this was complicated to find
          // a piece can be placed at the same legal position as a number piece if it fits around it
          // i.e., an upperLeftEl can be at 0,0 and a bigLowerRightEl can also be at 0,0 because
          // they don't overlap on occupied positions
          // so to make this algo work, add the offset to the first On Position in the first row
          // so we can validate that pos1 comes before pos2 comes before pos3, barring line clearings
          val pos1 = plc1.loc.row * boardSize + plc1.loc.col + plc1.piece.offSetToFirstOnPosition
          val pos2 = plc2.loc.row * boardSize + plc2.loc.col + plc2.piece.offSetToFirstOnPosition
          val pos3 = plc3.loc.row * boardSize + plc3.loc.col + plc3.piece.offSetToFirstOnPosition

          /* 
             the first piece can be any position from 0 to positions - 2
             the second piece can be the first piece position + 1 up to positions - 1
                 if a lines is cleared after the first piece then the second piece can be 0 to positions - 1
             the third piece, can be the second position + 1 up to positions
                  if a line is cleared after the second piece then thd third number can be any position
             */
          val start2 = if (plc1.clearedLines)
            0
          else
            pos1 + 1

          val start3 = if (plc2.clearedLines)
            0
          else
            pos2 + 1

          // using the offsets, we should _never_ any positions that map to the same value
          assert(pos1 != pos2 && pos2 != pos3 && pos3 != pos1)

          (pos1 <= context.boardPositions - 2) && (pos2 >= start2 && pos2 < context.boardPositions - 1) && (pos3 >= start3)

        }

        // we have to count this one if it clears lines as this can happen on any permutation
        linesCleared || mustUpdateForThisPermutation
      }

      def updateBest(simulation: Simulation): Unit = {

        def simulationIsBetterThanBest: Boolean = best.get.weightedSum < simulation.weightedSum

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

      def updateSimulation(plcArray: Array[PieceLocCleared], board: Board): Unit = {
        val id = simulationCount.inc()
        // reverse is expensive - only do this when showing in the UX
        val simulation = Simulation(plcArray, board, id)
        updateBest(simulation)
      }

      def getLegal(board: Board, piece: Piece): GenSeq[Loc] = {
        if (context.parallel)
          board.legalPlacements(piece).par
        else
          board.legalPlacements(piece)
      }

      val piece = pieces(depth) // .head

      val paralegal: GenSeq[Loc] = getLegal(board, piece)

      def simulationHandler(loc: Loc) = {

        val result = placeMe(piece, board, loc)
        val boardCopy = result.board
        val plc = result.plc

        val locPieceHash = getLocPieceHash(loc, piece, locPieceHashAcc)

        val plcArray = plcArrayAcc :+ plc

        val score = result.score + scoreAcc
        // recurse
        // if we have already cleared lines then propagate that so we don't pay the freight
        // in "isUpdatable" where it's an expensive calculation
        val linesCleared = if (linesClearedAcc) linesClearedAcc else plc.clearedLines

        // by adding a depth tracker and not using a call to tail to get the remaining pieces for the simulation
        // on the macbookair, went from 6,500/s calls to simulationHandler up to 8,000/s

        if ( depth < ( Game.numPiecesInRound - 1 ) /*pieces.tail.nonEmpty*/) {

          createSimulations(boardCopy, pieces/*.tail*/, linesCleared, plcArray, score, locPieceHash, depth + 1)

        } else {

          // following is for testing that we are simulating all possible simulations
          // this records legal positions
          // it's okay to keep pumping legalPositions in on all threads
          // as this will be turned into a set that highlights the unique positions tested
          if (context.simulationSelfTest)
            synchronized { this.legalPositionsSelfTest += locPieceHash }

          // only add simulation when we've reached the last legal location on this path
          if (isUpdatable(plcArray, linesCleared)) {

            // this completes the test of validating we are simulating all legal positions
            // this records all simulations
            if (context.simulationSelfTest)
              synchronized { this.simulatedPositionsSelfTest += locPieceHash }

            // this is the one score that happens outside
            boardCopy.roundScore = score

            updateSimulation(plcArray, boardCopy)

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
        updateSimulation(plcArrayAcc, board)
      }
    }

    createSimulations(board, pieces, linesClearedAcc = false, plcArrayAcc = Array(), scoreAcc = 0, locPieceHashAcc = 0, 0)

    def emptySimulation: Simulation = Simulation(Array(), this.board, 0)

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

    board.place(piece, loc, updateColor = true)

    // increment piece usage
    piece.usage.inc()

    // score me baby
    // score.inc(piece.pointValue)

    val unclearedBoard = Board.copy(this.board)

    val linesClearedResult = board.clearLines

    rowsCleared.inc(linesClearedResult.rows)
    colsCleared.inc(linesClearedResult.cols)
    val total = linesClearedResult.rows + linesClearedResult.cols

    val pieceScore = piece.pointValue + context.lineClearingScore(total)
    score.inc(pieceScore)

    val clearedBoard = Board.copy(this.board)

    this.board.roundScore += pieceScore
    clearedBoard.roundScore = this.board.roundScore

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

    val minScoreLength = 5 // to allow for 4 digits and a comma
    // this is a whole lot of string construction... should it be here? or should we split pieceHandler and boardResultsString into two separate operations
    val scoreLength = pieceHandlerInfo.score.shortLabel.length.max(minScoreLength)
    val valuesWidth = scoreLength.max(minScoreLength)

    val boardSpacer = " ".repeat(2)
    val labelWidth = context.specification.maxFeatureKeyLength

    val scoreLabel = "score".leftAlignedPadded(labelWidth).appendColon + pieceHandlerInfo.score.label(valuesWidth).green + boardSpacer

    val newScore = Array(
      scoreLabel,
      "cleared rows".leftAlignedPadded(labelWidth).appendColon + pieceHandlerInfo.rowsCleared.label(valuesWidth) + boardSpacer,
      "cleared cols".leftAlignedPadded(labelWidth).appendColon + pieceHandlerInfo.colsCleared.label(valuesWidth) + boardSpacer,
      " ".leftAlignedPadded(labelWidth) + " ".repeat("".appendColon.length) + " ".repeat(valuesWidth) + boardSpacer,
      ("features".leftAlignedPadded(labelWidth) + " ".repeat("".appendColon.length) + " ".repeat(valuesWidth)).underline + boardSpacer

    )

    val newBoardResultsString = context.specification.spec.values.zip(pieceHandlerInfo.clearedBoard.boardScore.scores)
      .map {
        case (optFactor, scoreComponent) =>
          optFactor.key.leftAlignedPadded(labelWidth).appendColon + scoreComponent.intValue.label(valuesWidth) + boardSpacer
      }.toArray

    val scoreInfoLength = newScore.length + newBoardResultsString.length
    // whichever is longer score info or board length
    val totalHeight = scoreInfoLength.max(boardSize)

    val remainingScoreLines = (scoreInfoLength until totalHeight)
      .map(_ => " ".repeat(labelWidth + valuesWidth + boardSpacer.length + 2)).toArray

    val newResults: Array[String] = newScore ++ newBoardResultsString ++ remainingScoreLines

    val appendToBoard = "\n" + (boardSize until totalHeight)
      .map(_ => " ".repeat(boardSize * 2 - 1 + boardSpacer.length) + "\n").mkString

    val newBoard = (pieceHandlerInfo.unclearedBoard.show + appendToBoard) splice newResults

    val finalBoard = if (pieceHandlerInfo.index == Game.numPiecesInRound) newBoard splice (this.board.show + appendToBoard).split("\n") else newBoard

    val placingLabel: String = "Placing " + pieceHandlerInfo.index.firstSecondThirdLabel + " ".repeat(2)
    val placingBuffer = placingLabel.length
    val pieceWidth = pieceHandlerInfo.piece.cols * 2 - 1

    val pieceArray: Array[String] = pieceHandlerInfo.piece.show.split("\n").map(each => each + " ".repeat(placingBuffer - pieceWidth - 2))

    val placingHeader = Array(placingLabel) ++ pieceArray

    val remainingPieceLines = (placingHeader.length to gamePieces.tallestPiece).map(_ => " ".repeat(placingBuffer)).toArray
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
      def getGameTimeInfo: Array[String] = {

        val gameElapsedNanoseconds = gameTimer.elapsedNanoseconds.toFloat
        timerSimulationResultsString.resume()
        timerPrintHeader.resume()
        timerGetPlacePiecesResultsString.resume()
        timerPrintRoundResults.pause()
        val standardTimingsString = Array(
          "",
          ("game " + multiGameStats.gameNumber.shortLabel + " round " + rounds.shortLabel + " results").header,
          // duration info
          "game elapsed time".label + gameTimer.elapsedLabel,
          "non simulation time".label + nonSimulationTimer.elapsedLabelMs + (nonSimulationTimer.elapsedNanoseconds / gameElapsedNanoseconds).percentLabel,
          "post simulation time".label + postTimer.elapsedLabelMs + (postTimer.elapsedNanoseconds / gameElapsedNanoseconds).percentLabel,
          "place pieces results".label + timerGetPlacePiecesResultsString.elapsedLabelMs + (timerGetPlacePiecesResultsString.elapsedNanoseconds / gameElapsedNanoseconds).percentLabel,
          //"get round results".label + timerGetRoundResultsString.elapsedLabelMs + (timerGetRoundResultsString.elapsedNanoseconds / gameElapsedNanoseconds).percentLabel,
          "simulation results".label + timerSimulationResultsString.elapsedLabelMs + (timerSimulationResultsString.elapsedNanoseconds / gameElapsedNanoseconds).percentLabel,
          "print header".label + timerPrintHeader.elapsedLabelMs + (timerPrintHeader.elapsedNanoseconds / gameElapsedNanoseconds).percentLabel,
          "print round results".label + timerPrintRoundResults.elapsedLabelMs + (timerPrintRoundResults.elapsedNanoseconds / gameElapsedNanoseconds).percentLabel
        )
        timerSimulationResultsString.pause()
        timerPrintHeader.pause()
        timerGetPlacePiecesResultsString.pause()
        timerPrintRoundResults.resume()

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
          "race cond. handled".label + gameStats.totalRaceConditionOnBest.label + " (" + lastRoundInfo.rcChangedCountBest.shortLabel + ")",
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

      print(s)
    }
  }

  private def logWeights(): Unit = {

    if (context.logJSON) {
      val weightJSON = context.specification.getWeightsJSON
      context.jsonLogger.info(weightJSON)
    }

  }

  private def logRound(
    results:          List[SimulationInfo],
    best:             Simulation,
    pieceHandlerInfo: List[PieceHandlerInfo],
    colorGridBitMask: BigInt
  ): Unit = {

    // log pieces - format is in ./src/main/resources/sample.json
    // only log if asked to do so via command line parameter -j, --logjson

    if (context.logJSON) {

      def getScores(scores: Array[FeatureScore]): String = {
        scores.map { scoreComponent =>

          (
            "name".jsonNameValuePair(scoreComponent.key.doubleQuote) +
            "intVal".jsonNameValuePair(scoreComponent.intValue) +
            "normalizedVal".jsonNameValuePair(scoreComponent.normalizedValue) +
            "weightedVal".jsonNameValuePairLast(scoreComponent.weightedValue)

          ).curlyBraces

        }.mkString(JSONFormats.delimiter)
      }

      val selectedPieces = pieceHandlerInfo.map { info =>

        (
          "name".jsonNameValuePair(info.piece.name.doubleQuote) +
          "row".jsonNameValuePair(info.loc.row) +
          "col".jsonNameValuePair(info.loc.col) +
          "rowsCleared".jsonNameValuePair(info.rowsCleared) +
          "colsCleared".jsonNameValuePairLast(info.colsCleared)

        ).curlyBraces

      }.mkString(JSONFormats.delimiter)

      val endOfRoundScores = getScores(this.board.boardScore.scores)

      val permutations = if (context.abridgedLogs) "" // don't include permuations in abridged results
      else
        results.zipWithIndex.map {
          case (p, i) =>

            val winner = p.best == best

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

            (index + winnerString + pieceArrayString + permutationScores).curlyBraces

        }.mkString(JSONFormats.delimiter)

      val s = (
        "type".jsonNameValuePair("Round".doubleQuote) +
        "round".jsonNameValuePair(rounds.value) +
        "score".jsonNameValuePair(score.value) +
        "linesCleared".jsonNameValuePair(rowsCleared.value + colsCleared.value) +
        /* "gamePieceSeed".jsonNameValuePair(gameSeed) + */ // you can get this from the file name
        "beginOfRoundColorGridBitMask".jsonNameValuePair(colorGridBitMask.toString.doubleQuote) +
        "endOfRoundBitMask".jsonNameValuePair(this.board.grid.asBigInt.toString.doubleQuote) +
        "selectedPieces".jsonNameValuePair(selectedPieces.squareBracket) +
        (if (context.abridgedLogs) // only include permutations when not abridged
          "endOfRoundScores".jsonNameValuePairLast(endOfRoundScores.squareBracket)
        else {
          "endOfRoundScores".jsonNameValuePair(endOfRoundScores.squareBracket) +
            "permutations".jsonNameValuePairLast(permutations.squareBracket)
        })
      ).curlyBraces

      this.lastRoundJSON = s

      context.jsonLogger.info(s)
    }

  }

}

object Game {

  val numPiecesInRound: Int = 3

}