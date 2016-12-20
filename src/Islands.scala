/**
 * Created by nathan on 12/19/16.
 * methods to create an islands array showing contiguous blocks of space
 */

import scala.collection.mutable.ListBuffer

object Islands {

  def makeIslands(layout: Array[Array[Cell]]): List[List[(Int, Int)]] = {

    // once again, i can't get the recursive solution so resorting to imperative :(
    val islands = new ListBuffer[ListBuffer[(Int, Int)]]
    val visited = new ListBuffer[(Int, Int)]

    def unoccupied(loc: (Int, Int)): Boolean = !layout(loc._1)(loc._2).occupied

  /*  def isPartOfIsland(loc: (Int, Int), island: ListBuffer[(Int, Int)]): Boolean = {
      island.exists { existing =>

        val i = existing._1
        val j = existing._2

        val x = loc._1
        val y = loc._2

        ((j == y) && ((i == (x - 1)) || (i == (x + 1)))) || ((i == x) && ((j == (y - 1)) || (j == (y + 1))))

      }
    }

    def addToExistingIsland(loc: (Int, Int)): Unit = {

      // filter for membership in a particular island
      // if it's in there, then return islands mapped with the updated island
      val filteredIslands = islands.filter(l => isPartOfIsland(loc, l))

      if (filteredIslands.isEmpty)
        islands append ListBuffer(loc)
      else {
        filteredIslands(0).append(loc)
      }
    }*/

    def isSafe(loc: (Int, Int)): Boolean = {

      val i = loc._1
      val j = loc._2

      val inbounds = (i >= 0 && i < layout.length && j >= 0 && j < layout(0).length)
      if (!inbounds)
        return false

      if (!unoccupied(loc))
        return false

      if (visited.contains(loc))
        return false

      true

    }

    def dfs(loc: (Int, Int), newIsland:ListBuffer[(Int,Int)]): Unit = {

      // if we haven't visited, say we have
      visited append loc

      newIsland append loc


      // call dfs in all directions - only acting on valid locations - until it can't find any more islands
      val directions = List((-1, 0), (0, -1), (1, 0), (0, 1))

      for {
        offset <- directions
        tryLoc = (loc._1 + offset._1, loc._2 + offset._2)
        if (isSafe(tryLoc))
      } dfs(tryLoc, newIsland)

    }

    def makeIslandsHelper(locs: List[(Int, Int)]): List[List[(Int, Int)]] = {

      // If an unoccupied cell is not visited yet,
      // then new island found
      for {loc <- locs
          if unoccupied(loc)}
          {
            if (visited.isEmpty || !visited.contains(loc)) {
              val newIsland = new ListBuffer[(Int,Int)]
              islands append newIsland
              dfs(loc, newIsland)
            }
          }

      islands.map(island => island.toList).toList
    }

    val locs = GameUtil.getLocationsList[Cell](layout)

    makeIslandsHelper(locs)

  }

}
