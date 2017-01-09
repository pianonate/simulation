/**
 * Created by nathan on 12/10/16.
 * Game will hold a reference to the current board and will also invoke simulations
 *
 * Todo: add game # to prefix each Score so you can no which iteration your in when you leave this
 *       plaing over night
 *
 * Todo: at end of game you can often place one or two pieces, but we don't place any - find out why
 *
 * TODO: Kevin suggests assigning values from 0 to 5 to 0 on both axes and minimize for placement on higher valued squares
 *       I.e., stay out of the middle
 *
 * TODO: Kevin suggests only running simulations where stuff exists or on the left col or top row depending on piece length
 *       only expand out to other simulations if you HAVE to - this might provide a lot of savings
 *       for example if you place 3 singletons on an empty board, there's no reason to move the pieces further than 3 out from the origin - huge savings
 *
 * Todo: save every move in a game so you can replay it if it's awesome
 *
 * Todo: start looking at hte points your game loses at - find out where you disagree with the choices it made
 *
 * Todo: introduce random seed and then follow that particular seeded game through to completion -
 *       you can optimize one game play at a time rather than trying to run them over and over
 *       and hope for something better!
 *
 * Todo:  save datasets of all of the top say 90% of boards for each of your stats in games where you lose.  then keep track of whether or not you lose in the next
 *        round for each of them.  then you'll have a dataset to run a machine learning algorithm of of our stats to better pick the best options
 */

import Game._

object GameOver extends Exception

class Game(val highScore: Int, context: Context) {

  private val BYATCH_THRESHOLD = 550000 // your system has some cred if it is doing more than this number of simulations / second

  private val CONTINUOUS_MODE = true // set to false to have the user advance through each board placement by hitting enter

  private val board: Board = new Board(Board.BOARD_SIZE)
  private val gamePieces: Pieces = new Pieces

  private val score = Counter()
  private val rowsCleared = Counter()
  private val colsCleared = Counter()
  private val rounds = Counter()

  import scala.collection.mutable.ListBuffer
  private val simulationsPerSecond = new ListBuffer[Int]

  // maximum allowed simulations for this game
  // mostly used in profiling to limit the number of simulations to a reasonable number
  val maxSimulations: Int = context.maxSimulations

  def run(machineHighScore: Int): (Int, Int, Int) = {

    val gameDuration = new Timer

    try {

      do {

        roundhandler(machineHighScore)

      } while (CONTINUOUS_MODE || (!CONTINUOUS_MODE && (Console.in.read != 'q')))

    } catch {

      case GameOver => // normal game over
      case e: Throwable =>
        println("abnormal run termination:\n" + e.toString)
        // todo: find out what type of error assert is throwing and match it
        //       currently assert is used at least in pieceHandler to ensure occupiedcounts are
        //       not messed up
        throw new IllegalStateException()

    }

    showGameOver(gameDuration)

    // return the score and the number of rounds to Main - where such things are tracked across game instances
    (score.value, rounds.value, simulationsPerSecond.max)

  }

  private def roundhandler(machineHighScore: Int) = {

    // get 3 random pieces
    val pieces = getPiecesForRound

    // show the pieces in the order they were randomly chosen
    showPieces(pieces)

    val best = getBestPermutation(pieces)

    println(
      "     Chosen: "
        + piecesToString(best.plcList.map(plc => plc.piece))
        + " -"
        + best.getSimulationResultsString(None)
    )

    // zip the piece location cleared list it's index so we don't have to keep a
    // global track of placed pieces
    best.plcList.zipWithIndex.foreach(
      plc => pieceHandler(
        plc._1.piece,
        plc._1.loc,
        plc._2 + 1,
        machineHighScore
      )
    )

    if (best.pieceCount < 3) {
      throw GameOver
    }

    showBoardFooter()
  }

  private def getBestPermutation(pieces: List[Piece]): Simulation = {
    // set up a test of running through all orderings of piece placement (permutations)
    // and for each ordering, try all combinations of legal locations
    // the best score as defined by Simulations.compare after trying all legal locations is chosen

    //one thing to note - (thanks Kevin) - and this seems like a duh in retrospect...
    // order doesn't matter if we don't clear any lines.
    // in a no-lines-cleared situation, it's only the board state at the end of one permutation simulation that matters
    // thus - simulate one run

    val permutations = pieces
      .permutations
      .toList

    val simulateHead: Simulation = pieceSequenceSimulation(permutations.head)

    // if there were cleared lines in this simulation then continue
    if (simulateHead.plcList.exists(plc => plc.clearedLines)) {
      val simulateTail: List[Simulation] = permutations.tail.map(pieces => pieceSequenceSimulation(pieces))
      (simulateHead :: simulateTail).sorted.head
    } else {

      simulateHead
    }

  }

  // use this method to return specific pieces under specific circumstances
  // under normal conditions if (false
  // just return a random set of 3 pieces
  private def getPiecesForRound: List[Piece] = {

    val pieces = {

      //List.fill(3)(gamePieces.getNamedPiece("VLine5"))
      //gamePieces.getNamedPieces("BigLowerLL","BigLowerLL","BigLowerLL")
      List.fill(3)(gamePieces.getRandomPiece)

    }

    pieces
  }

  private def pieceSequenceSimulation(pieces: List[Piece]): Simulation = {

    val simulationDuration = new Timer
    val simulationCount = Counter()

    //return the board copy and the number of lines cleared
    def placeMe(piece: Piece, theBoard: Board, loc: (Int, Int)): (Board, PieceLocCleared) = {
      val boardCopy = Board.copy("simulationBoard", theBoard)
      boardCopy.place(piece, loc)
      val cleared = boardCopy.clearLines()
      // return the board with an instance of a PieceLocCleared class
      (boardCopy, PieceLocCleared(piece, loc, (cleared._1 + cleared._2) > 0))

    }

    def getLegal(board: Board, piece: Piece) = {
      if (context.serialMode)
        board.legalPlacements(piece)
      else
        board.legalPlacements(piece).par
    }

    // i can't see how to not have this ListBuffer
    // i can accumulate locations because you generate one each time through the recursion
    // but you only store the last simulation - so there's nothing to accumulate...
    val listBuffer = new ListBuffer[Simulation]

    def createSimulations(board: Board, pieces: List[Piece], plcAccumulator: List[PieceLocCleared]): Unit = {

      val piece = pieces.head

      val paralegal = getLegal(board, piece)

      def processLoc(loc: (Int, Int)) = {
        // maxSimulations is configured at runtime
        // if we are profiling it uses a smaller number so tracing doesn't slow things down that we can't get a few rounds
        if (simulationCount.value < maxSimulations) {

          val result = placeMe(piece, board, loc)
          val (boardCopy, plc) = result

          if (pieces.tail.nonEmpty) {
            // recurse
            createSimulations(boardCopy, pieces.tail, plc :: plcAccumulator)
          } else {

            val plcList = plc :: plcAccumulator
            val simulation = Simulation(plcList.reverse, boardCopy)
            simulationCount.inc // increment simulation counter

            synchronized {
              // only append simulation when we've reached the last legal location on this path
              listBuffer append simulation
            }
          }
        }
      }

      paralegal.foreach(processLoc)

    }

    createSimulations(board, pieces, List())

    val options = {
      val grouped = listBuffer.toList.groupBy(_.pieceCount)

      // we only want to return simulations that have all 3 pieces placed if there are any
      if (grouped.contains(3)) grouped(3)
      else if (grouped.contains(2)) grouped(2)
      else if (grouped.contains(1)) grouped(1)
      else List(Simulation(List(), this.board))

    }

    val best = options.sorted.head

    // invert the default comparison and take that as the result for worst
    // god i love the Ordered trait
    val worst = options.sortWith(_ > _).head

    // now we know how long this one took - don't need to include the time to show or return it
    val elapsed = simulationDuration.elapsed

    // extracted for readability
    showSimulationResults(pieces, simulationCount, best, worst, elapsed)

    best

  }

  private def showSimulationResults(pieces: List[Piece], simulationCount: Counter, best: Simulation, worst: Simulation, elapsed: Long) = {

    val largeValuesFormat = "%,5d"

    val simulationCountString = largeValuesFormat.format(simulationCount.value)

    val perSecond = if (elapsed > 0) (simulationCount.value * 1000) / elapsed else 0

    // keep track of best per second value to get a sense of how things are performing across simulations
    simulationsPerSecond append perSecond.toInt

    val durationString = largeValuesFormat.format(elapsed) + "ms"
    val perSecondString = largeValuesFormat.format(perSecond)

    println(
      "permutation: "
        + piecesToString(pieces)
        + " -"
        + best.getSimulationResultsString(Some(worst.results))
        + " - simulations: " + simulationCountString + " in " + durationString
        + " ("
        + perSecondString + "/second"
        + (if (perSecond > BYATCH_THRESHOLD) " b-yatch" else "")
        + ")"
    )
  }

  // todo:  can this be turned into an implicit for a List[Piece].toString call?  if so, that would be nice
  private def piecesToString(pieces: List[Piece]): String = pieces.map(_.name).mkString(", ")

  private def pieceHandler(piece: Piece, loc: (Int, Int), index: Int, machineHighScore: Int): Unit = {

    val currentOccupied = board.getOccupiedPositions

    val curRowsCleared = rowsCleared.value
    val curColsCleared = colsCleared.value

    println("\nAttempting piece: " + index + "\n" + piece.show + "\n")

    // a function designed to throw GameOver if it receives no valid locations
    board.place(piece, loc)

    // increment piece usage
    piece.usage.inc

    score.inc(piece.pointValue)

    // placing a piece puts underlines on it to highlight it
    println(board.show)

    // so now clear any previously underlined cells for the next go around
    board.clearPieceUnderlines(piece, loc)

    handleLineClearing()

    val rowsClearedThisRun = rowsCleared.value - curRowsCleared
    val colsClearedThisRun = colsCleared.value - curColsCleared
    val positionsCleared = ((rowsClearedThisRun + colsClearedThisRun) * 10) - (rowsClearedThisRun * colsClearedThisRun)

    val expectedOccupiedCount = currentOccupied + piece.pointValue - positionsCleared

    assert(expectedOccupiedCount == board.getOccupiedPositions, "Expected occupied: " + expectedOccupiedCount + " actual occupied: " + board.occupiedCount)

    // todo - get this from Simulation
    // you'll have to evaluate all current values for board and then
    // pass them as some collection to Simulation.specification and ask it to
    // provide the results back in the right order

    val results = board.results

    println(
      "Score: "
        + getScoreString(numberFormatShort, score.value)
        + " (" + getScoreString(numberFormatShort, highScore) + ")"
        + " (" + getScoreString(numberFormatShort, machineHighScore) + ")"
        + " -" + Simulation.getSpecResultsString(board.results)

    )

  }

  private def handleLineClearing() = {

    val result = board.clearLines()

    if (result._1 > 0 || result._2 > 0) {

      def printMe(i: Int, s: String): Unit = if (i > 0) println("cleared " + i + " " + s + (if (i > 1) "s" else ""))

      printMe(result._1, "row")
      printMe(result._2, "column")

      // show an updated board reflecting the cleared lines
      println("\n" + board.show)

      rowsCleared.inc(result._1)
      colsCleared.inc(result._2)

    }

    score.inc((result._1 + result._2) * board.layout.length)

  }

  private def showBoardFooter() = {

    // increment the number of rounds
    rounds.inc

    println("\nAfter " + "%,d".format(rounds.value) + " rounds"
      + " - rows cleared: " + "%,d".format(rowsCleared.value)
      + " - columns cleared: " + "%,d".format(colsCleared.value)
      + " - best simulations per second: " + "%,d".format(simulationsPerSecond.max))

    if (!CONTINUOUS_MODE)
      println("type enter to place another piece and 'q' to quit")
  }

  private def showGameOver(gameDuration: Timer) = {

    println

    println(gamePieces.usageString)

    println("\nGAME OVER!!\n")

    println(labelFormat.format("Final Score") + getScoreString(numberFormat, score.value))
    println(labelNumberFormat.format("Rounds", rounds.value))
    println(labelNumberFormat.format("Rows Cleared", rowsCleared.value))
    println(labelNumberFormat.format("Cols Cleared", colsCleared.value))
    println(gameDuration.showElapsed)
  }

  private def showPieces(pieces: List[Piece]): Unit = {

    // we'll need the height of the tallest piece as a guard for shorter pieces where we need to
    // print out spaces as placeholders.  otherwise array access in the for would be broken
    // if we did some magic to append fill rows to the pieces as strings array...
    val tallestPieceHeight = pieces.map(piece => piece.rows).reduceLeft((a, b) => if (a > b) a else b)

    // because we're not printing out one piece, but three across, we need to split
    // the toString from each piece into an Array.  In this case, we'll create a List[Array[String]]
    val piecesToStrings = pieces map { piece =>

      val a = piece.show.split('\n')
      if (a.length < tallestPieceHeight)
        a ++ Array.fill(tallestPieceHeight - a.length)(piece.printFillString) // fill out the array
      else
        a // just return the array

    }

    println("\nRound: " + numberFormatShort.format(rounds.value + 1) + " - Candidate pieces:")

    // turn arrays into a list so you can transpose them
    // transpose will create a list of 1st rows, a list of 2nd rows, etc.
    // then print them out - across and then newline delimit
    piecesToStrings.map(a => a.toList)
      .transpose
      .foreach { l => print(l.mkString); println }

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

  def showGameStart(): Unit = {

    //todo - ask Simulation for its current strategy and interpolate
    //       into the list of factors considered in the output string below

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

    val descriptions = Simulation.getSpecDescriptions

    println(
      begin + "\n" + descriptions + end
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
