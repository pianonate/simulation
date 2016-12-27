/**
 * Created by nathan on 12/19/16.
 * methods to create an islands array showing contiguous blocks of unoccupied space
 */

import scala.collection.mutable.ListBuffer

object Islands {

  def findIslands(layout: Array[Array[Cell]], locations: Array[(Int, Int)]): List[Int] = {

    def getVisitedArray = Array.fill(layout.length)(Array.fill(layout.length)(false))

    // once again, i can't get the recursive solution so resorting to imperative :(
    val islands = new ListBuffer[Int]

    // call dfs in all directions - only acting on valid locations - until it can't find any more islands
    val directions = Array((-1, 0), (0, -1), (1, 0), (0, 1))

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
    def dfs(loc: (Int, Int), dfsVisited: Array[Array[Boolean]]) /* newIsland: ListBuffer[(Int, Int)])*/ : Array[Array[Boolean]] /*List[(Int, Int)] */ = {

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

    def findIslandsHelper(locations: Array[(Int, Int)]): List[Int] /*List[List[(Int, Int)]]*/ = {

      // If an unoccupied cell is not visited yet,
      // then new island found

      // switched to while to go faster than for comprehension
      var i = 0

      while (i < locations.length) {
        val loc = locations(i)

        // if unoccupied and not visited already by dfs
        if ((!occupied(loc)) && (!isVisited(loc, visited))) {

          val island = dfs(loc, getVisitedArray)
          val count = island.map(a => a.count(_ == true)).sum
          islands append count
        }

        i += 1

      }

      islands.toList
    }

    findIslandsHelper(locations)

  }

}