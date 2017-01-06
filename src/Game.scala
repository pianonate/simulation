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

//    todo consider making this a service and you show the actual game simulation in an ios app or browser with angular. which is even more useful
object GameOver extends Exception

class Game(val highScore: Int, context: Context) {

  // Todo: create a manifest object that specifies the ordering and participation of
  //       various algorithms used in this simulation (openLines, maximizer, etc.
  //       this will allow you to quickly turn them off or on (or potentially provide them at the command line)
  private val BYATCH_THRESHOLD = 550000 // your system has some cred if it is doing more than this number of simulations / second

  private val CONTINUOUS_MODE = true // set to false to have the user advance through each board placement by hitting enter

  private val board: Board = new Board(Board.BOARD_SIZE)
  private val gamePieces: Pieces = new Pieces

  private val score = Counter()
  private val rowsCleared = Counter()
  private val colsCleared = Counter()
  private val rounds = Counter()
  private val placed = Counter()

  import scala.collection.mutable.ListBuffer
  private val simulationsPerSecond = new ListBuffer[Int]

  val maxSimulations: Int = context.maxSimulations

  def run(machineHighScore: Int): (Int, Int, Int) = {

    // val t1 = System.currentTimeMillis()
    val duration = new Timer

    try {

      do {

        /*     val l = gamePieces.getNamedPiece("BigLowerLL")
        board.place(l,(0,0))
        board.place(l,(4,0))
        board.place(l,(7,0))
        println(board.show)
        board.openLines
*/

        // get 3 random pieces
        val pieces = getPiecesForRound

        // show the pieces in the order they were randomly chosen
        showPieces(pieces)

        val best = getBestPermutation(pieces)

        println(getSimulationResultsString("     Chosen: " + piecesToString(best.plcList.map(plc => plc.piece)), best, None))

        best.plcList.foreach(plc => handleThePiece(best, plc.piece, plc.loc, machineHighScore))

        if (best.pieceCount < 3)
          throw GameOver

        showBoardFooter()

      } while (CONTINUOUS_MODE || (!CONTINUOUS_MODE && (Console.in.read != 'q')))

    } catch {

      case GameOver => // normal game over
      case e: Throwable =>
        println("abnormal run termination:\n" + e.toString)
        // todo: find out what type of error assert is throwing and match it
        throw new IllegalStateException()

    }

    showGameOver(duration)

    // return the score and the number of rounds to Main - where such things are tracked across game instances

    // this would be more clear - then Main's only purpose is to be the application entry point
    (score.value, rounds.value, simulationsPerSecond.max)

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
      // this code provides specific pieces for the (probable) last iteration
      // was used as a condition to debug a situation in the end game
      // if (board.occupiedCount > 50)

      // used to test a few rounds...
      /*if (rounds.head ==1)
        Piece.getNamedPieces("VerticalLine5", "LowerLeftEl", "HorizontalLine3")
      else if (rounds.head == 2)
        Piece.getNamedPieces("Box", "VerticalLine3", "BigUpperRightEl")
      else if (rounds.head == 3)
       Piece.getNamedPieces("BigUpperLeftEl", "BigBox", "VerticalLine5")
      else*/

      //List.fill(3)(gamePieces.getNamedPiece("VLine5"))
      //gamePieces.getNamedPieces("BigLowerLL","BigLowerLL","BigLowerLL")
      List.fill(3)(gamePieces.getRandomPiece)

    }
    pieces
  }

  case class PieceLocCleared(piece: Piece, loc: (Int, Int), clearedLines: Boolean)

  case class Simulation(plcList: List[PieceLocCleared], board: Board) extends Ordered[Simulation] {

    val pieceCount: Int = plcList.length

    val boardCount: Int = board.occupiedCount
    val maximizerCount: Int = board.legalPlacements(Game.maximizer).length
    val (openLines: Int, openContiguous: Int) = board.openLines
    val islandMax: Int = board.islandMax
    val neighborCounts: Array[Int] = board.neighborCount
    val fourNeighbors: Int = neighborCounts(4)

    // the following provides tuple ordering to ordered to make the tuple comparison work
    import scala.math.Ordered.orderingToOrdered

    override def toString: String = this.plcList.map(plc => plc.piece.name).mkString(", ")

    // new algo - lowest boardCount followed by largest island
    // format: OFF
    def compare(that: Simulation): Int = {
                (this.boardCount, that.maximizerCount, this.fourNeighbors, that.openContiguous, that.islandMax, that.openLines)
        .compare(that.boardCount, this.maximizerCount, that.fourNeighbors, this.openContiguous, this.islandMax, this.openLines)

      // format: ON

    }

  }

  private def pieceSequenceSimulation(pieces: List[Piece]): Simulation = {

    val t1 = System.currentTimeMillis()
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

      if (grouped.contains(3)) grouped(3)
      else if (grouped.contains(2)) grouped(2)
      else if (grouped.contains(1)) grouped(1)
      else List(Simulation(List(), this.board))

    }

    // with the new optimization, sorted should be the slowest thing as it lazy val's the fields of the Simulation
    val best = options.sorted.head

    // invert the default comparison and take that as the result for worst
    val worst = options.sortWith(_ > _).head

    val t2 = System.currentTimeMillis
    // todo: create a timer class that starts, stops and does a toString
    val duration = t2 - t1

    val largeValuesFormat = "%,5d"

    val simulationCountString = largeValuesFormat.format(simulationCount.value)

    val perSecond = if (duration > 0) (simulationCount.value * 1000) / duration else 0

    simulationsPerSecond append perSecond.toInt

    val durationString = largeValuesFormat.format(duration) + "ms"
    val sPerSecond = largeValuesFormat.format(perSecond)

    val prefix = "permutation: " + piecesToString(pieces)

    println(
      getSimulationResultsString(prefix, best, Some(worst))
        + " - simulations: " + simulationCountString + " in " + durationString
        + " (" + sPerSecond + "/second" + (if (perSecond > BYATCH_THRESHOLD) " b-yatch" else "") + ")"
    )

    best

  }

  private def getSimulationResultsString(prefix: String, best: Simulation, worst: Option[Simulation] = None): String = {

    val greenify = (isGreen: Boolean, v: AnyVal, valFormat: String, label: String, labelFormat: String) => {
      val result = valFormat.format(v)
      labelFormat.format(label) + (if (isGreen) Game.GREEN + result + Game.SANE else result)
    }

    val openFormat = "%2d"
    val parenFormat = " (" + openFormat + ")"
    val labelFormat = " %s: "
    val longLabelFormat = "     " + labelFormat

    val occupiedLabel = "occupied"
    val maximizerLabel = "maximizer"
    val contiguousLabel = "contiguous open lines"
    val openLabel = "openRowsCols"
    val islandMaxLabel = "islandMax"
    val fourNeighborsLabel = "four neighbors"

    val results = (best, worst) match {
      case (b: Simulation, w: Some[Simulation]) =>
        (
          greenify(b.boardCount < w.get.boardCount, b.boardCount, openFormat, occupiedLabel, labelFormat)
          + greenify(false, w.get.boardCount, parenFormat, "", "")

          + greenify(b.maximizerCount > w.get.maximizerCount, b.maximizerCount, openFormat, maximizerLabel, labelFormat)
          + greenify(false, w.get.maximizerCount, parenFormat, "", "")

          + greenify(b.fourNeighbors < w.get.fourNeighbors, b.fourNeighbors, openFormat, fourNeighborsLabel, labelFormat)
          + greenify(false, w.get.fourNeighbors, parenFormat, "", "")

          + greenify(b.openContiguous > w.get.openContiguous, b.openContiguous, openFormat, contiguousLabel, labelFormat)
          + greenify(false, w.get.openContiguous, parenFormat, "", "")

          + greenify(b.islandMax > w.get.islandMax, b.islandMax, openFormat, islandMaxLabel, labelFormat)
          + greenify(false, w.get.islandMax, parenFormat, "", "")

          + greenify(b.openLines > w.get.openLines, b.openLines, openFormat, openLabel, labelFormat)
          + greenify(false, w.get.openLines, parenFormat, "", "")

        )
      case (b: Simulation, None) =>
        (
          greenify(true, b.boardCount, openFormat, occupiedLabel, labelFormat)
          + greenify(true, b.maximizerCount, openFormat, maximizerLabel, longLabelFormat)
          + greenify(true, b.fourNeighbors, openFormat, fourNeighborsLabel, longLabelFormat)
          + greenify(true, b.openContiguous, openFormat, contiguousLabel, longLabelFormat)
          + greenify(true, b.islandMax, openFormat, islandMaxLabel, longLabelFormat)
          + greenify(true, b.openLines, openFormat, openLabel, longLabelFormat)

        )

    }

    prefix + " -" + results

  }

  // todo:  can this be turned into an implicit for a List[Piece].toString call?  if so, that would be nice
  private def piecesToString(pieces: List[Piece]): String = pieces.map(_.name).mkString(", ")

  private def handleThePiece(best: Simulation, piece: Piece, loc: (Int, Int), machineHighScore: Int): Unit = {

    val currentOccupied = board.occupiedCount
    val curRowsCleared = rowsCleared.value
    val curColsCleared = colsCleared.value

    println("\nAttempting piece: " + ((placed.value % 3) + 1) + "\n" + piece.show + "\n")

    // a function designed to throw GameOver if it receives no valid locations
    board.place(piece, loc)

    placed.inc
    piece.usage.inc

    score.incrementMultiple(piece.pointValue)

    // placing a piece puts underlines on it to highlight it
    println(board.show)

    // so now clear any previously underlined cells for the next go around
    board.clearPieceUnderlines(piece, loc)

    handleLineClearing()

    val rowsClearedThisRun = rowsCleared.value - curRowsCleared
    val colsClearedThisRun = colsCleared.value - curColsCleared
    val positionsCleared = ((rowsClearedThisRun + colsClearedThisRun) * 10) - (rowsClearedThisRun * colsClearedThisRun)

    val expectedOccupiedCount = currentOccupied + piece.pointValue - positionsCleared

    assert(expectedOccupiedCount == board.occupiedCount, "Expected occupied: " + expectedOccupiedCount + " actual occupied: " + board.occupiedCount)

    println("Score: " + getScoreString(numberFormatShort, score.value) + " (" + getScoreString(numberFormatShort, highScore) + ")" + " (" + getScoreString(numberFormatShort, machineHighScore) + ")"
      + " - occupied: " + board.occupiedCount
      + " - contiguous lines: " + board.openLines._2
      + " - open lines: " + board.openLines._1
      + " - maximizer positions available: " + board.legalPlacements(Game.maximizer).length
      + " - largest contiguous unoccupied: " + board.islandMax)

  }

  private def handleLineClearing() = {

    val result = board.clearLines()

    if (result._1 > 0 || result._2 > 0) {

      def printMe(i: Int, s: String): Unit = if (i > 0) println("cleared " + i + " " + s + (if (i > 1) "s" else ""))

      printMe(result._1, "row")
      printMe(result._2, "column")

      // show an updated board reflecting the cleared lines
      println("\n" + board.show)

      rowsCleared.incrementMultiple(result._1)
      colsCleared.incrementMultiple(result._2)

    }

    score.incrementMultiple((result._1 + result._2) * board.layout.length)

  }

  private def showGameOver(duration: Timer) = {

    println

    println(gamePieces.usageString)

    println("\nGAME OVER!!\n")

    println(labelFormat.format("Final Score") + getScoreString(numberFormat, score.value))
    println(labelNumberFormat.format("Rounds", rounds.value))
    println(labelNumberFormat.format("Rows Cleared", rowsCleared.value))
    println(labelNumberFormat.format("Cols Cleared", colsCleared.value))
    println(duration.toString) // toString stops the timer
  }

  private def showBoardFooter() = {
    val bestPerSecond = simulationsPerSecond.max

    rounds.inc

    println("\nAfter " + "%,d".format(rounds.value) + " rounds"
      + " - rows cleared: " + "%,d".format(rowsCleared.value)
      + " - columns cleared: " + "%,d".format(colsCleared.value)
      + " - best simulations per second: " + "%,d".format(bestPerSecond))

    if (!CONTINUOUS_MODE)
      println("type enter to place another piece and 'q' to quit")
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

  // one of the optimizations is to ensure that the maximum number of
  // maximum pieces can fit on a board from all the boards simulated in the permutation of a set of pieces
  // apparently it's important that this be declared after Game.CYAN is declared above :)
  private val maximizer = new Box("Maximizer", Game.CYAN, 3, 0)

  def getScoreString(formatString: String, score: Int): String = GREEN + formatString.format(score) + SANE

  def showGameStart(): Unit = {

    //todo - rewrite to include what you do with the occupied, etc. - also this can provide a key for it

    println("GAME START")
    println("\nThis game works by first selecting 3 pieces from the set of all possible pieces.")
    println("Then it will try all possible orderings (permutations) of placements of those 3 pieces")
    println("(up to six permutations will be selected depending on whether there are any duplicates).")
    println("\nThen for each permutation, it will try each position on the board for the first piece")
    println("and will determine the outcome of clearing lines for that piece piece, then it will place")
    println("the second piece at all possible locations and check the outcome of clearing lines for the second")
    println("and finally it will check the third piece at all possible locations and check the outcome")
    println("of clearing lines for this last piece.")
    println("\nEach of these combinations of placements will then store the maximum number of legal positions")
    println("available for this piece (called the maximizer):\n")
    println(maximizer.toString)
    println("\nThe game uses the maximizer because it generally is a good choice for making sure there is")
    println("plenty of space available.")
    println("\nThe combination of piece placements with the least number of board positions occupied and the most")
    println("number of legal positions for the maximizer piece is the one that will be selected to play.")
    println("Each combination for each permutation is called a 'simulation'.")

  }

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

  }

}
