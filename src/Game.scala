/**
 * Created by nathan on 12/10/16.
 * Game will hold a reference to the current board and will also invoke simulations
 *
 * Todo: at end of game you can often place one or two pieces, but we don't place any - find out why
 *
 * Todo: show points per second, per minute and hours,minutes,seconds to a new high score for the current game
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

import Game._

object GameOver extends Exception

class Game(context: Context, gameInfo: GameInfo) {

  private[this] val CONTINUOUS_MODE = true // set to false to have the user advance through each board placement by hitting enter

  private[this] val board: Board = new Board(Board.BOARD_SIZE, context.specification)
  private[this] val gamePieces: Pieces = new Pieces

  private[this] val score = Counter()
  private[this] val rowsCleared = Counter()
  private[this] val colsCleared = Counter()
  private[this] val rounds = Counter()

  case class PerformanceInfo(simulations: Long, unsimulatedSimulations:Long, duration: Long, perSecond: Int)

  import scala.collection.mutable.ListBuffer
  private[this] val performanceInfoList = new ListBuffer[PerformanceInfo]

  private[this] val gameDuration = new GameTimer

  def totalSimulations: Long = {
    performanceInfoList.map(_.simulations).sum
  }

  def run: (Int, Int, Int) = {

    try {

      do {

        // todo figure out how to capture a pause character without having to hit return
        roundHandler()

      } while (CONTINUOUS_MODE || (!CONTINUOUS_MODE && (Console.in.read != 'q')))

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

    // todo - add a case class
    // return the score and the number of rounds to Main - where such things are tracked across game instances
    (score.value, rounds.value, if (context.replayGame) 0 else performanceInfoList.map(_.perSecond).max)

  }

  private def roundHandler() = {

    // todo - this shit is way too convoluted can you simplify the replayGame scenario?

    // increment the number of rounds
    rounds.inc()

    val instrumentedPlcList: List[PieceLocCleared] = if (context.replayGame) {
      val next3 = context.takeReplayPiecesForRound.toList
      if (next3.length < 3)
        throw GameOver
      else
        next3
    } else
      List()

    // get either instrumented or 3 random pieces
    val pieces = if (context.replayGame) instrumentedPlcList.map(_.piece) else getPiecesForRound

    // show the pieces in the order they were randomly chosen
    showPieces(pieces)

    // get the results so you can show them the new way (no results for instrumented
    val results = if (context.replayGame && context.ignoreSimulation) List[SimulationInfo]() else getSimulationResults(pieces)

    // sort out the best of the best results
    val chosen: Simulation = if (context.replayGame && context.ignoreSimulation)
      Simulation(instrumentedPlcList, this.board, context.specification.length)
    else
      results.map(_.best).sorted.head

    // todo use futures here to allow for outputting interesting things while simulations are running
    if (context.show) {
      println("done thinking")
      if (context.replayGame && context.ignoreSimulation)
        println("instrumented game - skipping results")
      else
        println(context.specification.getImprovedResultsString(results, chosen))
    }

    val chosenList = if (context.replayGame && context.ignoreSimulation) instrumentedPlcList else chosen.plcList

    // zip the piece location cleared list it's index so we don't have to keep a
    // global track of placed pieces
    chosenList.zipWithIndex.foreach(
      plc => pieceHandler(
        plc._1.piece,
        plc._1.loc,
        plc._2 + 1
      )
    )

    showBoardFooter(gameInfo.gameCount: Int)

    if (chosen.pieceCount < 3 || (rounds.value == context.stopGameAtRound)) {
      throw GameOver
    }

  }

  private def getSimulationResults(pieces: List[Piece]): List[SimulationInfo] = {
    // set up a test of running through all orderings of piece placement (permutations)
    // and for each ordering, try all combinations of legal locations
    // the best score as defined by Simulations.compare after trying all legal locations is chosen

    val permutations = pieces
      .permutations
      .toList

    val contextualizedPermutations = {
      if (context.serialMode)
        permutations
      else
        permutations.toArray.par
    }

    if (context.show) print(Bullshit.getBullshit)

    val duration = new GameTimer

    val result = contextualizedPermutations
      .zipWithIndex
      .map(pieces => pieceSequenceSimulation(pieces._1, pieces._2, contextualizedPermutations.length))
      .toList

    val elapsed = duration.elapsed
    val sum = result.map(_.simulationCount).sum.toDouble
    val unsimulated = result.map(_.unsimulatedCount).sum


    // todo - output unsimulated as %
    performanceInfoList append PerformanceInfo(sum.toLong + unsimulated, unsimulated, elapsed, ((sum + unsimulated.toDouble )/ elapsed * 1000).toInt)

    result

  }

  // use this method to return specific pieces under specific circumstances
  // under normal conditions if (false
  // just return a random set of 3 pieces
  private def getPiecesForRound: List[Piece] = {

    val pieces = {

      //List.fill(3)(gamePieces.getNamedPiece("VLine5"))
      // List(Pieces.box,Pieces.h3line,Pieces.upperLeftEl)
      List.fill(3)(gamePieces.getRandomPiece)

    }

    pieces
  }

  private def pieceSequenceSimulation(pieces: List[Piece], permutationIndex: Int, totalPermutations: Int): SimulationInfo = {

    if (context.show) print(".")

    val simulationDuration = new GameTimer
    val simulationCount = Counter()
    val unsimulatedCount = Counter()

    // i can't see how to not have this Array
    // i can accumulate locations because you generate one each time through the recursion
    // but you only store the last simulation - so there's nothing to accumulate...
    // val simulations = new Array[Simulation](maxSimulations)

    // keep these populated as weo
    // as this is better than sorting at the end as it allows for a parallel sort
    // as it the compare is called on each thread while walking through the legalPlacements(piece).par
    // provided by getLegal below
    var best: Option[Simulation] = None
    var worst: Option[Simulation] = None

    //return the board copy and the number of lines cleared
    def placeMe(piece: Piece, theBoard: Board, loc: Loc): (Board, PieceLocCleared) = {
      val boardCopy = Board.copy("simulationBoard", theBoard)
      boardCopy.place(piece, loc)
      val cleared = boardCopy.clearLines()
      val isCleared = (cleared._1 + cleared._2) > 0

      // return the board with an instance of a PieceLocCleared class
      (boardCopy, PieceLocCleared(piece, loc, isCleared))

    }

    def getLegal(board: Board, piece: Piece): GenSeq[Loc] = {
      if (context.serialMode)
        board.legalPlacements(piece)
      else
        board.legalPlacements(piece).par
    }

    def createSimulations(board: Board, pieces: List[Piece], plcAccumulator: List[PieceLocCleared]): Unit = {

      val piece = pieces.head

      val paralegal: GenSeq[Loc] = getLegal(board, piece)

      def simulationHandler(loc: Loc) = {
        // maxSimulations is configured at runtime
        // if we are profiling it uses a smaller number so tracing doesn't slow things down that we can't get a few rounds

        def updateSimulations(simulation: Simulation) = {

          def updateBestAndWorst(): Unit = {

            best match {
              case a: Some[Simulation] =>
                val comparison = simulation.compare(a.get)
                if (comparison < 0)
                  best = Some(simulation)
              case _ => best = Some(simulation)
            }

            worst match {
              case a: Some[Simulation] =>
                val comparison = simulation.compare(a.get)
                if (comparison > 0)
                  worst = Some(simulation)
              case _ => worst = Some(simulation)
            }

          }
          synchronized {
            // apparently another thread updating simulationCount to be incremented before we enter here
            if (simulationCount.value < context.maxSimulations) {
              updateBestAndWorst()
              simulationCount.inc()
            }
          }

        }

        if (simulationCount.value < context.maxSimulations) {

          val result = placeMe(piece, board, loc)
          val (boardCopy, plc) = result

          if (pieces.tail.nonEmpty) {
            // recurse
            createSimulations(boardCopy, pieces.tail, plc :: plcAccumulator)
          } else {

            // TODO - walk through this code!!!
            def isUpdatable(plcList: List[PieceLocCleared]): Boolean = {
              var updatable = false
              var i = 0
              val length = plcList.length
              while (i < length && !updatable) {
                if (plcList(i).clearedLines)
                  updatable = true

                i += 1
              }
              //val driverLoc = plcList(length - 1).loc
              val index = loc.row * 10 + loc.col
              val mustUpdateForThisPermutation = (index % totalPermutations) == permutationIndex
              updatable || mustUpdateForThisPermutation
            }
            // right now we never get to the third simulation if it can't fit
            // so it would be good to return a 2 piece simulation or 1 piece simulation if they can be returned
            val plcList = plc :: plcAccumulator

            // only add simulation when we've reached the last legal location on this path
            if (isUpdatable(plcList) /*true*/) {
              val simulation = Simulation(plcList.reverse, boardCopy, context.specification.length)
              updateSimulations(simulation)
            }
            else {
              synchronized { unsimulatedCount.inc() }
            }

          }
        }
      }

      paralegal.foreach(simulationHandler)

    }
    createSimulations(board, pieces, List())

    def emptySimulation: Simulation = Simulation(List(), this.board, context.specification.length)

    /*    val options: Array[Simulation] = {
      val grouped = simulations.splitAt(simulationCount.value)._1.groupBy(_.pieceCount)

      if (grouped.contains(3)) grouped(3)
      else if (grouped.contains(2))
        grouped(2)
      else if (grouped.contains(1))
        grouped(1)
      else
        Array(emptySimulation)
    }*/

    // now we know how long this one took - don't need to include the time to show or return it
    val elapsed = simulationDuration.elapsed

    // extracted for readability
    val result = SimulationInfo(pieces, simulationCount.value, unsimulatedCount.value,  best.getOrElse(emptySimulation), worst.getOrElse(emptySimulation), elapsed)

    result

  }

  private def pieceHandler(piece: Piece, loc: Loc, index: Int): Unit = {

    // todo - get rid of piece.cellShowFunction - it's got to work better than this
    if (context.show) println("\nAttempting piece " + index + " at " + loc.show + "\n" + piece.show(piece.cellShowFunction) + "\n")

    board.place(piece, loc)

    // increment piece usage
    piece.usage.inc()

    // score me baby
    score.inc(piece.pointValue)

    if (context.show) println(getShowBoard)

    handleLineClearing()

    if (context.show) println(
      "Game: " + getScoreString(numberFormatShort, gameInfo.gameCount) + " - "
        + "Score: "
        + getScoreString(numberFormatShort, score.value)
        + " (" + getScoreString(numberFormatShort, gameInfo.sessionHighScore) + ")"
        + " (" + getScoreString(numberFormatShort, gameInfo.machineHighScore) + ")"
        + " -" + context.specification.getBoardResultString(board.results)

    )

  }

  private def getShowBoard = {
    board.show(board.cellShowFunction) /*.split("\n").zip(board.boardNeighborsArray).map(s => s._1 + s._2.mkString(" ")).mkString("\n")*/
  }

  private def handleLineClearing() = {

    val result = board.clearLines()

    if (result._1 > 0 || result._2 > 0) {

      def printMe(i: Int, s: String): Unit = if (i > 0) println("cleared " + i + " " + s + (if (i > 1) "s" else ""))

      if (context.show) {
        printMe(result._1, "row")
        printMe(result._2, "column")

        // show an updated board reflecting the cleared lines
        println("\n" + getShowBoard)
      }

      rowsCleared.inc(result._1)
      colsCleared.inc(result._2)

    }

    score.inc(result._1 * Board.BOARD_SIZE + result._2 * Board.BOARD_SIZE - result._1 * result._2)

  }

  private def showBoardFooter(gameCount: Int) = {

    def avg(xs: ListBuffer[PerformanceInfo]): Double = {
      val (sum, length) = xs.map(_.perSecond).foldLeft((0l, 0))({ case ((s, l), x) => (x + s, 1 + l) })
      val result = sum / length
      result
    }

    def formatNumber(value: Int) = numberFormatShort.format(value)

    if (!(context.replayGame && context.ignoreSimulation)) {
      val average: Double = avg(performanceInfoList)

      val last = performanceInfoList.last

      if (context.show) println("\nGame: " + formatNumber(gameCount) + " - "
        + "After " + formatNumber(rounds.value) + " rounds"
        + " - simulations: " + formatNumber(last.simulations.toInt)
        + " (" + formatNumber(last.unsimulatedSimulations.toInt) + ")" // todo - fix the toInt bs
        /*+ " - total simulations" + formatNumber(this.totalSimulations.toInt)*/
        + " - duration: " + formatNumber(last.duration.toInt) + "ms"
        + " - " + formatNumber(last.perSecond) + "/s"
        + " - best: " + formatNumber(performanceInfoList.map(_.perSecond).max) + "/s"
        + " (avg: " + formatNumber(average.toInt) + "/s)"
        + " - elapsed time: " + gameDuration.showElapsed
        + " (" + gameInfo.totalTime.showElapsed + ")"
      // + " " + numberFormatShort.format(simulationsPerSecond.sum)
      )
    }

    if (!CONTINUOUS_MODE)
      println("type enter to place another piece and 'q' to quit")
  }

  private def showGameOver() = {
    if (context.show) {
      println

      println(gamePieces.usageString)

      println("\nGAME OVER!!\n")

      println(labelFormat.format("Final Score") + getScoreString(numberFormat, score.value))
      println(labelNumberFormat.format("Rounds", rounds.value))
      println(labelNumberFormat.format("Rows Cleared", rowsCleared.value))
      println(labelNumberFormat.format("Cols Cleared", colsCleared.value))
      println(labelFormat.format("Game Elapsed Time") + gameDuration.showElapsed)
    }
  }

  private def showPieces(pieces: List[Piece]): Unit = {

    if (context.show) {

      // we'll need the height of the tallest piece as a guard for shorter pieces where we need to
      // print out spaces as placeholders.  otherwise array access in the for would be broken
      // if we did some magic to append fill rows to the pieces as strings array...
      val tallestPieceHeight = pieces.map(piece => piece.rows).reduceLeft((a, b) => if (a > b) a else b)

      // because we're not printing out one piece, but three across, we need to split
      // the toString from each piece into an Array.  In this case, we'll create a List[Array[String]]
      val piecesToStrings = pieces map { piece =>

        val a = piece.show(piece.cellShowFunction).split('\n')
        if (a.length < tallestPieceHeight)
          a ++ Array.fill(tallestPieceHeight - a.length)(piece.printFillString) // fill out the array
        else
          a // just return the array

      }

      println("\nRound: " + numberFormatShort.format(rounds.value) + " - Candidate pieces:")

      // turn arrays into a list so you can transpose them
      // transpose will create a list of 1st rows, a list of 2nd rows, etc.
      // then print them out - across and then newline delimit
      piecesToStrings.map(a => a.toList)
        .transpose
        .foreach { l => print(l.mkString); println }
    }
  }

}

object Game {

  // these are used in the end game
  val labelFormat = "%-24s: "
  val numberFormat = "%,7d"
  val numberFormatShort = "%,d"
  val labelNumberFormat: String = labelFormat + numberFormat

  // Color code strings from:
  // http://www.topmudsites.com/forums/mud-coding/413-java-ansi.html
  val SANE = "\u001B[0m"

  val ESCAPE = "\u001B"
  val HIGH_INTENSITY = "\u001B[1m"
  val LOW_INTENSITY = "\u001B[2m"

  val ITALIC = "\u001B[3m"
  val UNDERLINE = "\u001B[4m"
  val BLINK = "\u001B[5m"
  val RAPID_BLINK = "\u001B[6m"
  val REVERSE_VIDEO = "\u001B[7m"
  val INVISIBLE_TEXT = "\u001B[8m"

  val BLACK = "\u001B[30m"
  val RED = "\u001B[31m"
  val GREEN = "\u001B[32m"
  val YELLOW = "\u001B[33m"
  val BLUE = "\u001B[34m"
  val MAGENTA = "\u001B[35m"
  val CYAN = "\u001B[36m"
  val WHITE = "\u001B[37m"

  val BRIGHT_BLACK = "\u001B[90m"
  val BRIGHT_RED = "\u001B[91m"
  val BRIGHT_GREEN = "\u001B[92m"
  val BRIGHT_YELLOW = "\u001B[93m"
  val BRIGHT_BLUE = "\u001B[94m"
  val BRIGHT_MAGENTA = "\u001B[95m"
  val BRIGHT_CYAN = "\u001B[96m"
  val BRIGHT_WHITE = "\u001B[97m"

  val BACKGROUND_BLACK = "\u001B[40m"
  val BACKGROUND_RED = "\u001B[41m"
  val BACKGROUND_GREEN = "\u001B[42m"
  val BACKGROUND_YELLOW = "\u001B[43m"
  val BACKGROUND_BLUE = "\u001B[44m"
  val BACKGROUND_MAGENTA = "\u001B[45m"
  val BACKGROUND_CYAN = "\u001B[46m"
  val BACKGROUND_WHITE = "\u001B[47m"

  def getScoreString(formatString: String, score: Int): String = GREEN + formatString.format(score) + SANE

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

  /*
  // print the character colors that we have available to us
  def printPossibleColors(): Unit = {
    for (i <- 30 to 37) {
      val code = i.toString
      print(f"\u001b[38;5;$code%sm$code%3s")
    }

    println("")

    for (i <- 90 to 97) {
      val code = i.toString
      print(f"\u001b" +
        f"[38;5;$code%sm$code%3s")
    }

    println

  }*/

}
