/**
 * Created by nathan on 12/9/16.
 */
sealed abstract class Piece { val color: String; val layout: Array[Array[Boolean]]  }

/*
 Singleton
 ■
*/

class Singleton extends Piece { val color = Console.BLUE;  val layout: Array[Array[Boolean]] = Array(Array(true))  }

/*
 H2Line
 ■ ■
*/

class H2Line extends Piece { val color = Console.BLUE;  val layout: Array[Array[Boolean]] = Array(Array(true, true))  }
class V2Line extends Piece { val color = Console.BLUE;  val layout: Array[Array[Boolean]] = Array(Array(true), Array(true))  }

/*
 H3Line
 ■ ■ ■
*/

class H3Line extends Piece { val color = Console.BLUE;  val layout: Array[Array[Boolean]] = Array(Array(true, true, true))  }
class V3Line extends Piece { val color = Console.BLUE;  val layout: Array[Array[Boolean]] = Array(Array(true),  Array(true),  Array(true))  }

/*
 H4Line
 ■ ■ ■ ■
*/

class H4Line extends Piece { val color = Console.BLUE;  val layout: Array[Array[Boolean]] = Array(Array(true, true, true, true))  }
class V4Line extends Piece { val color = Console.BLUE;  val layout: Array[Array[Boolean]] = Array(Array(true),  Array(true),  Array(true), Array(true))  }

/*
 H5Line
 ■ ■ ■ ■ ■
*/

class H5Line extends Piece { val color = Console.BLUE;  val layout: Array[Array[Boolean]] = Array(Array(true, true, true, true, true))  }
class V5Line extends Piece { val color = Console.BLUE;  val layout: Array[Array[Boolean]] = Array(Array(true),  Array(true),  Array(true), Array(true), Array(true))  }

/*
 Box
 ■ ■
 ■ ■
 */

class Box extends Piece {  val color = Console.CYAN; val layout: Array[Array[Boolean]] =
  Array(
    Array(true, true),
    Array(true, true)
  )}

/*
 BigBox
 ■ ■ ■
 ■ ■ ■
 ■ ■ ■
 */

class BigBox extends Piece {  val color = Console.WHITE; val layout: Array[Array[Boolean]] =
  Array(
    Array(true, true, true),
    Array(true, true, true),
    Array(true, true, true)
  )}

/*
 Lower Left L:
 ■
 ■ ■

 */
class LowerLeftL extends Piece { val color = Console.GREEN; val layout: Array[Array[Boolean]] =
  Array(
    Array(true, false),
    Array(true, true)
  )}

class UpperLeftL extends Piece { val color = Console.GREEN; val layout: Array[Array[Boolean]] =
  Array(
    Array(true, true),
    Array(true, false)
  )}


class LowerRightL extends Piece { val color = Console.GREEN; val layout: Array[Array[Boolean]] =
  Array(
    Array(false, true),
    Array(true, true)
  )}

class UpperRightL extends Piece { val color = Console.GREEN; val layout: Array[Array[Boolean]] =
  Array(
    Array(true, true),
    Array(false, true)
  )}


/*
 Big Lower Left L
 ■
 ■
 ■ ■ ■

 */
class BigLowerLeftL extends Piece { val color = Console.MAGENTA; val layout: Array[Array[Boolean]] =
  Array(
    Array(true, false, false),
    Array(true, false, false),
    Array(true, true, true)
  )}

class BigUpperLeftL extends Piece { val color = Console.MAGENTA; val layout: Array[Array[Boolean]] =
  Array(
    Array(true, true, true),
    Array(true, false, false),
    Array(true, false, false)
  )}


class BigLowerRightL extends Piece { val color = Console.MAGENTA; val layout: Array[Array[Boolean]] =
  Array(
    Array(false, false, true),
    Array(false, false, true),
    Array(true, true, true)
  )}


class BigUpperRightL extends Piece { val color = Console.MAGENTA; val layout: Array[Array[Boolean]] =
  Array(
    Array(true, true, true),
    Array(false, false, true),
    Array(false, false, true)
  )}