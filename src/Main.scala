/**
 * Created by nathan on 12/9/16.
 * Main is just the stub to get it all off the ground
 */

import scala.collection.mutable.ListBuffer

object Main extends App {

 /* val layout = Array(
    Array(0,1,1,0),
    Array(0,0,1,1),
    Array(1,1,0,0),
    Array(0,1,0,0)
  )

  def makeIslands(islandValue: Int, matrix: Array[Array[Int]]): List[List[(Int,Int)]] = {

    // once again, i can't get the recursive solution so resorting to imperative :(
    val islands = new ListBuffer[ListBuffer[(Int,Int)]]
    val visited = new ListBuffer[(Int,Int)]

    def matrixValue(loc: (Int, Int)): Int = matrix(loc._1)(loc._2)

    def isPartOfIsland(loc: (Int,Int), island: ListBuffer[(Int,Int)]):Boolean = {
      island.exists{ existing =>

        val i = existing._1
        val j = existing._2

        val x = loc._1
        val y = loc._2

        ( (j==y) && ( (i == (x-1) ) || (i == (x+1)) ) ) || ( (i==x) && ( (j == (y-1) ) || (j == (y+1)) ) )

      }
    }

    def addToExistingIsland(loc: (Int, Int)):Unit = {

      // filter for membership in a particular island
      // if it's in there, then return islands mapped with the updated island
      val filteredIslands = islands.filter(l => isPartOfIsland(loc, l))

      if (filteredIslands.isEmpty)
        islands append ListBuffer(loc)
      else {
        filteredIslands(0).append(loc)
      }
    }

    def isSafe(loc: (Int, Int)): Boolean = {

      val i = loc._1
      val j = loc._2

      val inbounds = (i >= 0 && i < matrix.length && j>=0 && j < matrix(0).length)
      if (!inbounds)
        return false

      if (matrixValue(loc) != islandValue)
        return false



      true

    }

    def dfs(loc : (Int, Int) ): Unit = {
      if (visited.nonEmpty && visited.contains(loc)) return

      val isIsland = matrixValue(loc)==islandValue
      if (isIsland)
        visited.append(loc)
      else return

      if (islands.isEmpty)
        islands append ListBuffer(loc)
      else
        addToExistingIsland(loc)

      // call dfs in all directions - only acting on valid locations - until it can't find any more islands
      val directions = List((-1,0),(0,-1),(1,0),(0,1))

      for {offset <- directions
          tryLoc = (loc._1 + offset._1, loc._2 + offset._2)
          if (isSafe(tryLoc))
      } dfs(tryLoc)

    }

    def makeIslandsHelper(locs: List[(Int, Int)]):List[List[(Int,Int)]] = {

      locs match {
        case head :: tail => {
            dfs(head)
            makeIslandsHelper(tail)
          }
        case Nil => islands.toList.map(island => island.toList)
      }
    }

    val locs = GameUtil.getLocationsList[Int](layout)



    makeIslandsHelper(locs)

  }

  val a = makeIslands(0, layout)
  a.foreach {island => println("island: " + island.length); island.foreach(location=> println("(" + location._1 + "," + location._2 +")"))}
*/

  play()

  private def play(): Unit = {

    // todo - stash high scores in a file you can read at startup of the game
    // high score to date is 7,766

    // different than game continuous mode which simply
    // controls whether you hit enter to place the next piece
    // Main continuous mode means continuous play - you have to ctrl-c out of it
    val CONTINUOUS_MODE = true

    import scala.collection.mutable.ListBuffer
    val scores = new ListBuffer[Long]
    val rounds = new ListBuffer[Long]
    val simulationsPerSecond = new ListBuffer[Long]

    Game.showGameStart()

    // run the game, my friend
    do {
      val game = new Game()
      val results = game.run()

      scores.append(results._1)
      rounds.append(results._2)
      simulationsPerSecond.append(results._3)

      if (CONTINUOUS_MODE) {
        val highScore = scores.max
        val mostRounds = rounds.max
        val bestPerSecond = simulationsPerSecond.max

        val labelFormat = GameUtil.labelFormat
        val numberFormat = GameUtil.numberFormat

        println
        println
        println("MULTIPLE GAME STATS")
        println
        println(labelFormat.format("Games Played") + numberFormat.format(scores.size))
        println(labelFormat.format("High Score") + GameUtil.RED + numberFormat.format(highScore) + GameUtil.SANE)
        println(labelFormat.format("Most Rounds") + numberFormat.format(mostRounds))
        println(labelFormat.format("Most Simulations/Second") + numberFormat.format(bestPerSecond))

        println
        print("Starting new game in ")

        // countdown timer
        (1 to 10).reverse.foreach { i =>
          print(i + "...")
          Thread.sleep(1000)
        }

        println
        println("Go!")
        println
      }

    } while (CONTINUOUS_MODE)

  }

  //  printPossibleColors
  // print the character colors that we have available to us
  private def printPossibleColors(): Unit = {
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

