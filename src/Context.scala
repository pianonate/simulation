import com.sun.javaws.exceptions.InvalidArgumentException

/**
 * Created by nathan on 12/30/16.
 * parses command line and sets defaults
 *
 * Todo: just don't throw errors - shut things down in an orderly fashoin and output error at the command line
 */

class Context (args: Array[String]) {

  private val validArguments = Set("-maxSimulations")
  private val argMap: Map[String, String] = args.map(_.split(":")).map({case a if a.length==1 => (a(0),"true");case b if b.length==2 => (b(0),b(1))}).toMap

  argMap.keys.foreach(key => { if (!validArguments(key)) throw new InvalidArgumentException(Array(key))})

  // max simulations if you had 3 singletons chosen on an empty board:
  private val BOARD_UNOCCUPIED = Board.BOARD_SIZE * Board.BOARD_SIZE
  private val MAX_SIMULATION_ITERATIONS: Int = BOARD_UNOCCUPIED * (BOARD_UNOCCUPIED - 1) * (BOARD_UNOCCUPIED - 2)

  private def getIntArgValue(arg:String, default:Int) :Int = {
    try {
    if (argMap.contains(arg)) argMap(arg).toInt else default
    }
    catch {
      case e:java.lang.NumberFormatException => throw new IllegalArgumentException("Expected an Int but got this: " + e.getMessage())
      case unknownException:Throwable => throw unknownException
    }

  }

  lazy val maxSimulations = getIntArgValue("-maxSimulations", MAX_SIMULATION_ITERATIONS)



}
