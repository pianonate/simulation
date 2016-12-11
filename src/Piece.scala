/**
 * Created by nathan on 12/9/16.
 * represents pieces to try
 */

abstract sealed class Piece {
  val name: String
  val color: Ansi
  val layout: Array[Array[Cell]]

  // the score is the count of every on position in a piece
  lazy private val onPositions = for {
    i <- layout
    j <- i
    if j.occupied // filter out off positions
  } yield j

  lazy val score:Int = onPositions.length

  override def toString:String = {

    val s = new StringBuilder()
    for {row <- layout} {
      s ++= row.map(cell => cell.toString).foldRight(" ")((a, b) => a + " " + b) + "\n"
    }

    s.toString
  }

}

class BoardPiece(size: Int) extends Piece {
  val name = "Board"
  val color = Ansi.BrightBlack
  val layout = Piece.getBoardLayout(size, color)
}

/*
 Singleton
 ■
*/

class Singleton extends Piece {
  val name = "Singleton"
  val color = Ansi.Black
  val layout = Piece.getLayout(color, Array(Array(true)))
}


/*
 H2Lined
 ■ ■
*/

class H2Line extends Piece {
  val name = "H2Line"
  val color = Ansi.BrightYellow
  val layout = Piece.getLayout(color, Array(Array(true, true)))
}
class V2Line extends Piece {
  val name = "V2Line"
  val color = Ansi.BrightYellow
  val layout = Piece.getLayout(color, Array(Array(true), Array(true)))
}

/*
 H3Line
 ■ ■ ■
*/

class H3Line extends Piece {
  val name = "H3Line"
  val color = Ansi.Yellow
  val layout = Piece.getLayout(color, Array(Array(true, true, true)))
}
class V3Line extends Piece {
  val name = "V3Line"
  val color = Ansi.Yellow
  val layout = Piece.getLayout(color, Array(Array(true), Array(true), Array(true)))
}

/*
 H4Line
 ■ ■ ■ ■
*/

class H4Line extends Piece {
  val name = "H4Line"
  val color = Ansi.BrightRed
  val layout = Piece.getLayout(color, Array(Array(true, true, true, true)))
}

class V4Line extends Piece {
  val name = "V4Line"
  val color = Ansi.BrightRed
  val layout = Piece.getLayout(color, Array(Array(true), Array(true), Array(true), Array(true)))
}

/*
 H5Line
 ■ ■ ■ ■ ■
*/

class H5Line extends Piece {
  val name = "H5Line"
  val color = Ansi.Red
  val layout = Piece.getLayout(color, Array(Array(true, true, true, true, true)))
}

class V5Line extends Piece {
  val name = "V5Line"
  val color = Ansi.Red
  val layout = Piece.getLayout(color, Array(Array(true), Array(true), Array(true), Array(true), Array(true)))
}

/*
 Box
 ■ ■
 ■ ■
 */

class Box extends Piece {
  val name = "Box"
  val color = Ansi.Green
  val layout = Piece.getLayout(color,
    Array(
      Array(true, true),
      Array(true, true)
    )
  )
}

/*
 BigBox
 ■ ■ ■
 ■ ■ ■
 ■ ■ ■
 */

class BigBox extends Piece {
  val name = "BigBox"
  val color = Ansi.Cyan
  val layout = Piece.getLayout(color,
    Array(
      Array(true, true, true),
      Array(true, true, true),
      Array(true, true, true)
    )
  )
}

/*
 Lower Left L:
 ■
 ■ ■

 */
class LowerLeftL extends Piece {
  val name = "LowerLeftL"
  val color = Ansi.BrightCyan
  val layout = Piece.getLayout(color,
    Array(
      Array(true, false),
      Array(true, true)
    )
  )
}

class UpperLeftL extends Piece {
  val name = "UpperLeftL"
  val color = Ansi.BrightCyan
  val layout = Piece.getLayout(color,
    Array(
      Array(true, true),
      Array(true, false)
    )
  )
}

class LowerRightL extends Piece {
  val name = "LowerRightL"
  val color = Ansi.BrightCyan
  val layout = Piece.getLayout(color,
    Array(
      Array(false, true),
      Array(true, true)
    )
  )
}

class UpperRightL extends Piece {
  val name = "UpperRightL"
  val color = Ansi.BrightCyan
  val layout = Piece.getLayout(color,
    Array(
      Array(true, true),
      Array(false, true)
    )
  )
}

/*
 Big Lower Left L
 ■
 ■
 ■ ■ ■

 */
class BigLowerLeftL extends Piece {
  val name = "BigLowerLeftL"
  val color = Ansi.Blue
  val layout = Piece.getLayout(color,
    Array(
      Array(true, false, false),
      Array(true, false, false),
      Array(true, true, true)
    )
  )
}

class BigUpperLeftL extends Piece {
  val name = "BigUpperLeftL"
  val color = Ansi.Blue
  val layout = Piece.getLayout(color,
    Array(
      Array(true, true, true),
      Array(true, false, false),
      Array(true, false, false)
    )
  )
}

class BigLowerRightL extends Piece {
  val name = "BigLowerRightL"
  val color = Ansi.Blue
  val layout = Piece.getLayout(color,
    Array(
      Array(false, false, true),
      Array(false, false, true),
      Array(true, true, true)
    )
  )
}

class BigUpperRightL extends Piece {
  val name = "BigUpperRightL"
  val color = Ansi.Blue
  val layout = Piece.getLayout(color,
    Array(
      Array(true, true, true),
      Array(false, false, true),
      Array(false, false, true)
    )
  )
}

object Piece {



 val pieces = List[Piece](
    new Singleton ,
    new H2Line,
    new V2Line,
    new H3Line,
    new V3Line,
    new H4Line,
    new V4Line,
    new H5Line,
    new V5Line,
    new Box,
    new BigBox,
    new LowerLeftL,
    new UpperLeftL,
    new LowerRightL,
    new UpperRightL,
    new BigLowerLeftL,
    new BigUpperLeftL,
    new BigLowerRightL,
    new BigUpperRightL
  )


  def getLayout( color: Ansi, template: Array[Array[Boolean]]): Array[Array[Cell]] = {
    getLayoutImpl(color, false, template)
  }

  def getBoardLayout(size: Int, color: Ansi): Array[Array[Cell]] = {
    // this special getLayout is only called to construct the board - so the layout is always going to be
    // everything off
    getLayoutImpl(color, true, Array.ofDim[Boolean](size,size))
  }

  private def getLayoutImpl(color: Ansi, showUnoccupied: Boolean, template: Array[Array[Boolean]]): Array[Array[Cell]] = {

    // why can't Cell's be auto initialized based on this the following line the way that Ints and Booleans can
    val layout = Array.ofDim[Cell](template.length,template(0).length)

    for {
      i <- template.indices
      j <- template(0).indices
    } {
      val occupied = template(i)(j)
      if (occupied)
        layout(i)(j) = new Cell(occupied,color,showUnoccupied)
      else
        layout(i)(j) = new Cell(showUnoccupied=showUnoccupied)
    }

    layout
  }


}