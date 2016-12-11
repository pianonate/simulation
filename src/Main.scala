/**
 * Created by nathan on 12/9/16.
 */

import Console._

object Main extends App {

  10 to 10 foreach { (x: Int) =>
    val b = new Board(x)
    println(b.toString)
  }

}
