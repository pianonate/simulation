/**
 * Created by nathan on 12/19/16.
 * methods to create an islands array showing contiguous blocks of space
 */

import scala.collection.mutable.ListBuffer

object Islands {

  def findIslands(layout: Array[Array[Cell]], locations: List[(Int, Int)]): List[List[(Int, Int)]] = {

    // once again, i can't get the recursive solution so resorting to imperative :(
    val islands = new ListBuffer[ListBuffer[(Int, Int)]]

    val visited = Array.fill(layout.length)(Array.fill(layout.length)(false))

    def occupied(loc: (Int, Int)): Boolean = layout(loc._1)(loc._2).occupied

    def isVisited(loc: (Int, Int)): Boolean = (visited(loc._1)(loc._2) == true)

    def visit(loc: (Int, Int)): Unit = visited(loc._1)(loc._2) = true

    def isSafe(loc: (Int, Int)): Boolean = {

      val i = loc._1
      val j = loc._2

      val inbounds = i >= 0 && i < layout.length && j >= 0 && j < layout(0).length
      if (!inbounds)
        return false

      if (occupied(loc))
        return false

      if (isVisited(loc))
        return false

      true

    }

    def dfs(loc: (Int, Int), newIsland: ListBuffer[(Int, Int)]): Unit = {

      // if we haven't visited, say we have
      visit(loc)

      newIsland append loc

      // call dfs in all directions - only acting on valid locations - until it can't find any more islands
      val directions = Array((-1, 0), (0, -1), (1, 0), (0, 1))

      var i = 0
      while (i < directions.length) {
        val offset = directions(i)
        val tryLoc = (loc._1 + offset._1, loc._2 + offset._2)
        if (isSafe(tryLoc)) {
          dfs(tryLoc, newIsland)
        }

        i += 1
      }

      /*      for {
        offset <- directions
        tryLoc = (loc._1 + offset._1, loc._2 + offset._2)
        if isSafe(tryLoc)
      } dfs(tryLoc, newIsland)*/

    }

    def findIslandsHelper(locations: List[(Int, Int)]): List[List[(Int, Int)]] = {

      // If an unoccupied cell is not visited yet,
      // then new island found

      // switched to while to go faster than for comprehension
      var i = 0
      while (i < locations.length) {
        val loc = locations(i)
        // if unoccupied and not visited
        if ((!occupied(loc)) && (!isVisited(loc))) {
          val newIsland = new ListBuffer[(Int, Int)]
          islands append newIsland
          dfs(loc, newIsland)
        }
        i += 1
      }

      islands.map(island => island.toList).toList
    }

    findIslandsHelper(locations)

  }

}
