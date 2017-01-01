/**
 * Created by nathan on 12/19/16.
 * methods to create an islands array showing contiguous blocks of unoccupied space
 */

/*import scala.annotation.tailrec*/

object Islands {

  // call dfs in all directions - only acting on valid locations - until it can't find any more islands
  private val directions = Array((-1, 0), (0, -1), (1, 0), (0, 1))
  private val labels = getLabelsArray
  private val counter = Counter

  // start optimization
  // ExecutionTime: 27%, 2,542/second
  //
  // Moved getVisitedArray out and made a method that clones the original
  // ExecutionTime: 31%, 3,419/second (apparently execution time went up as a % but perf is better)
  //
  // getVisitedArray now returns an explicitly defined Array - dramatically speeds execution
  // ExecutionTime: 18%, 7,121/second
  //
  // round 2
  // ExecutionTime: 30%, 6,719/s (expected Time(ms) to go up as a % given other optimizations but strangey, per second regressed)
  //
  // now get rid of the ListBuffer appends
  // Execution time: 16%, 12,585/second 395% speed up from unoptimized
  //
  // next thing to do is to use the labelling mechanism of findCOmponents and also
  // return the max directly by only accumulating the largest island as you go
  // then you will only be updating one labels array and don't have to go through the max
  // calculation

  def findIslands(layout: Array[Array[Cell]], locations: Array[(Int, Int)]): List[Int] = {

    // keep track of all visited locations so you only start new islands where necessary
    val visited = getVisitedArray

    // is this location occupied?
    def occupied(loc: (Int, Int)): Boolean = layout(loc._1)(loc._2).occupied

    // has this location been visited?
    def isVisited(loc: (Int, Int), arr: Array[Array[Boolean]]): Boolean = arr(loc._1)(loc._2)

    // visit the location
    def visit(loc: (Int, Int), arr: Array[Array[Boolean]]): Unit = arr(loc._1)(loc._2) = true

    // is it safe to recurse into this location
    def isSafe(loc: (Int, Int), arr: Array[Array[Boolean]]): Boolean = {

      val i = loc._1
      val j = loc._2

      val inbounds = i >= 0 && i < layout.length && j >= 0 && j < layout(0).length

      if (!inbounds)
        return false

      if (isVisited(loc, arr))
        return false

      if (occupied(loc))
        return false

      true

    }

    // changing directions to an array sped things up significantly
    // also not accessing list.length during while loop
    // also eliminated appending the location to a ListBuffer
    // sped this up to 501 islands / second in profiler
    // from 319 / second prior.  57% speedup
    // changing locations to array from a list (constant indexed access time)
    // sped things up to 1,141 islands per second in the profiler - 258% faster
    def dfs(loc: (Int, Int), dfsVisited: Array[Array[Boolean]]) : Array[Array[Boolean]] = {

      // global visited so you don't create new islands where you've already been
      visit(loc, visited)
      // local visited to this particular island search
      visit(loc, dfsVisited)

      val (row, col) = loc
      var i = 0
      val length = directions.length
      while (i < length) {
        val (offsetRow, offsetCol) = directions(i)
        val tryLoc = (row + offsetRow, col + offsetCol)
        if (isSafe(tryLoc, dfsVisited)) {
          dfs(tryLoc, dfsVisited)
        }

        i += 1
      }

      dfsVisited

    }


    // start optimization run
    // Execution Time: 56%, 662/s
    //
    // turn count into a while loop summation using getIslandCount (new method)
    // Execution time: 36%, 1,917/s - 189% improvement
    //
    // getIslandCount remove lambda
    // Execution time: 22%, 3,602/s - 444% improvement over start
    //
    // Moved getVisitedArray out and made a method that clones the original
    // Execution time: 26%, 4,099/s - 502% improvement over start
    //
    // getVisitedArray now returns an explicitly defined Array - dramatically speeds execution
    // Execution time: 18%, 7,213/second - 901% improvement over start
    //
    // Round 2
    // Execution Time: 30%, 6,719/second - per second regression but that's ok - might just be session variances
    //
    // got rid of listBuffer via recursive accumulator
    // Execution Time: 16%, 12,823/s 1837% increase from start up optimization
    def findIslandsHelper(): List[Int] = {

      def getIslandCount(island: Array[Array[Boolean]]): Int = {
        /*island.map(a => a.count(_ == true)).sum*/


        @annotation.tailrec
        def islandCountloop(row: Int, col: Int, acc: Int): Int = {

          //val countMe = (r:Int, c:Int) => if (island(r)(c)) 1 else 0

          (row, col) match {
            case (-1, _) => acc
            case (_, 0) => islandCountloop(row - 1, island.length - 1, acc + (if (island(row)(col)) 1 else 0))
            case _ => islandCountloop(row, col - 1, acc + (if (island(row)(col)) 1 else 0))
          }
        }

        val result = islandCountloop(island.length - 1, island.length - 1, 0)
        result

      }

      @annotation.tailrec
      def findIslandsHelperLoop(index: Int, acc: List[Int]): List[Int] = {
        index match {

          case n: Int if n < locations.length => {
            // If an unoccupied cell is not visited yet,
            // then new island found

            // switched to while to go faster than for comprehension
            var i = n
            var found = false
            var count = 0

            while (i < locations.length && !found) {
              val loc = locations(i)

              // if unoccupied and not visited already by dfs
              if ((!occupied(loc)) && (!isVisited(loc, visited))) {

                val island = dfs(loc, getVisitedArray)

                count = getIslandCount(island)
                found = true
              }
              i += 1
            }
            if (count > 0)
              findIslandsHelperLoop(i, count :: acc)
            else
              acc
          }
          case _ => acc

        }
        /*islands.toList*/

      }

      findIslandsHelperLoop(0, List())

    }

    findIslandsHelper

  }

  /*private val visitedArray = Array.fill(Game.BOARD_SIZE)(Array.fill(Game.BOARD_SIZE)(false))*/
  // prior to this would clone visitedArray and return - but this one method
  // was taking up 15% of overall execution time.  So just return a predefined Array layout
  // as this takes about
  private def getVisitedArray = Array(
    Array(false,false,false,false,false,false,false,false,false,false),
    Array(false,false,false,false,false,false,false,false,false,false),
    Array(false,false,false,false,false,false,false,false,false,false),
    Array(false,false,false,false,false,false,false,false,false,false),
    Array(false,false,false,false,false,false,false,false,false,false),
    Array(false,false,false,false,false,false,false,false,false,false),
    Array(false,false,false,false,false,false,false,false,false,false),
    Array(false,false,false,false,false,false,false,false,false,false),
    Array(false,false,false,false,false,false,false,false,false,false),
    Array(false,false,false,false,false,false,false,false,false,false)
  )

  ////////////////////////


  def findComponents(layout: Array[Array[Cell]], locations: List[(Int, Int)]): Int = {
    import scala.collection.mutable.Queue

    // implemented One component at a time from
    // https://en.wikipedia.org/wiki/Connected-component_labeling

    val directions = Array((-1, 0), (0, -1), (1, 0), (0, 1))

    val labels = getLabelsArray // Array.fill(10)(Array.fill[Int](10)(0))
    val counter = Counter()
    val queue = Queue[(Int, Int)]()

    def getComponentCount(label: Int): Int = {
      /*island.map(a => a.count(_ == true)).sum*/

      @annotation.tailrec
      def componentCountLoop(row: Int, col: Int, acc: Int): Int = {

        //val countMe = (r:Int, c:Int) => if (island(r)(c)) 1 else 0

        (row, col) match {
          case (-1, _) => acc
          case (_, 0) => componentCountLoop(row - 1, labels.length - 1, acc + (if (labels(row)(col) == label) 1 else 0))
          case _ => componentCountLoop(row, col - 1, acc + (if (labels(row)(col) == label) 1 else 0))
        }
      }

      val result = componentCountLoop(labels.length - 1, labels.length - 1, 0)
      result

    }

    // is this location occupied?
    def occupied(loc: (Int, Int)): Boolean = layout(loc._1)(loc._2).occupied

    // has this location been visited?
    def isVisited(loc: (Int, Int)): Boolean = labels(loc._1)(loc._2) > 0

    // is it safe to recurse into this location
    def isSafe(loc: (Int, Int)): Boolean = {

      val i = loc._1
      val j = loc._2

      val inbounds = i >= 0 && i < layout.length && j >= 0 && j < layout(0).length

      if (!inbounds)
        return false

      if (isVisited(loc))
        return false

      if (occupied(loc))
        return false

      true

    }

    def findNeighbors(loc: (Int, Int), currentLabel: Int) = {

      var i = 0
      val length = directions.length
      while (i < length) {
        val (offsetRow, offsetCol) = directions(i)
        val tryLoc = (loc._1 + offsetRow, loc._2 + offsetCol)
        if (isSafe(tryLoc)) {
          label(tryLoc, currentLabel)
        }
        i += 1
      }

    }

    @annotation.tailrec
    def processQueue(currentLabel: Int): Unit = if (queue.nonEmpty) {
      val loc = queue.dequeue

      findNeighbors(loc, currentLabel)

      processQueue(currentLabel)
    }

    def label(loc: (Int, Int), label: Int): Unit = {
      labels(loc._1)(loc._2) = label
      queue.enqueue(loc)
      //println("I just  labelled " + loc + " with: " + label + " and enqueued")
    }

    def findComponentsLoop(locs: List[(Int, Int)], acc: Int): Int = {

      def processComponent(loc: (Int, Int)): Int = {
        val currentLabel = counter.inc + 1
        label(loc, currentLabel)
        processQueue(currentLabel)
        val count = getComponentCount(currentLabel)
        if (count > acc) count else acc
      }

      locs match {
        case head :: tail => {

          if (isSafe(head)) {

            val max: Int = processComponent(head)
            findComponentsLoop(tail, max)

          } else {

            findComponentsLoop(tail, acc)
          }
        }
        case Nil => acc
      }
    }

    findComponentsLoop(locations, 0)

  }

  private def getLabelsArray = Array(
    Array(0,0,0,0,0,0,0,0,0,0),
    Array(0,0,0,0,0,0,0,0,0,0),
    Array(0,0,0,0,0,0,0,0,0,0),
    Array(0,0,0,0,0,0,0,0,0,0),
    Array(0,0,0,0,0,0,0,0,0,0),
    Array(0,0,0,0,0,0,0,0,0,0),
    Array(0,0,0,0,0,0,0,0,0,0),
    Array(0,0,0,0,0,0,0,0,0,0),
    Array(0,0,0,0,0,0,0,0,0,0),
    Array(0,0,0,0,0,0,0,0,0,0)
  )

}