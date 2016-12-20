/**
 * Created by nathan on 12/19/16.
 * methods to create an islands array showing contiguous blocks of space
 */

import scala.collection.mutable.ListBuffer

object Islands {

  def findIslands(layout: Array[Array[Cell]]): List[List[(Int, Int)]] = {

    // once again, i can't get the recursive solution so resorting to imperative :(
    val islands = new ListBuffer[ListBuffer[(Int, Int)]]

    val locations = GameUtil.getLocationsList[Cell](layout)
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
      val directions = List((-1, 0), (0, -1), (1, 0), (0, 1))

      for {
        offset <- directions
        tryLoc = (loc._1 + offset._1, loc._2 + offset._2)
        if isSafe(tryLoc)
      } dfs(tryLoc, newIsland)

    }

    def findIslandsHelper(locations: List[(Int, Int)]): List[List[(Int, Int)]] = {

      // If an unoccupied cell is not visited yet,
      // then new island found
      for {
        loc <- locations
        if !occupied(loc)
      } {
        // pulled out of for comprehension as it seems to want to find out visited status for all locs before
        // diving into this foreach section...
        if (!isVisited(loc)) {
          val newIsland = new ListBuffer[(Int, Int)]
          islands append newIsland
          dfs(loc, newIsland)
        }
      }

      islands.map(island => island.toList).toList
    }

    // stuffed into a var for debugging
    val found = findIslandsHelper(locations)
    found

  }

}
