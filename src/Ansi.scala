/**
 * Usage:
 *
 * import Ansi._
 *
 * val msg0 = (Red and BgYellow) format ("Hello %s", name)
 * val msg1 = (Red + BgYellow)("Hello")
 *
 * Or also:
 *
 * val msg2 = Ansi.Red.and(Ansi.BgYellow).format("Hello %s", name)
 * val msg3 = Ansi.Blink("BOOM!")
 *
 * Or, if you are adverse to that, you can use the constants directly:
 *
 * val msg4 = new Ansi(Ansi.ITALIC, Ansi.GREEN).format("Green money")
 *
 * Or, even:
 *
 * val msg = Ansi.BLUE + "scientific"
 *
 * NOTE: Nothing stops you from combining multiple FG colors or BG colors,
 *       but only the last one will display.
 *
 * Thanks to: https://gist.github.com/dainkaplan/4651352
 */
object Ansi {

  // Color code strings from:
  // http://www.topmudsites.com/forums/mud-coding/413-java-ansi.html
  val SANE = "\u001B[0m"

  val HIGH_INTENSITY = "\u001B[1m"
  val LOW_INTENSITY = "\u001B[2m"

  val ITALIC = "\u001B[3m"
  val UNDERLINE = "\u001B[4m"
  val BLINK = "\u001B[5m"
  val RAPID_BLINK = "\u001B[6m"
  val REVERSE_VIDEO = "\u001B[7m"
  val INVISIBLE_TEXT = "\u001B[8m"

  val BLACK = "\u001B[30m"
  val RED = "\u001B[31m"
  val GREEN = "\u001B[32m"
  val YELLOW = "\u001B[33m"
  val BLUE = "\u001B[34m"
  val MAGENTA = "\u001B[35m"
  val CYAN = "\u001B[36m"
  val WHITE = "\u001B[37m"

  val BRIGHT_BLACK = "\u001B[90m"
  val BRIGHT_RED = "\u001B[91m"
  val BRIGHT_GREEN = "\u001B[92m"
  val BRIGHT_YELLOW = "\u001B[93m"
  val BRIGHT_BLUE = "\u001B[94m"
  val BRIGHT_MAGENTA = "\u001B[95m"
  val BRIGHT_CYAN = "\u001B[96m"
  val BRIGHT_WHITE = "\u001B[97m"

  val BACKGROUND_BLACK = "\u001B[40m"
  val BACKGROUND_RED = "\u001B[41m"
  val BACKGROUND_GREEN = "\u001B[42m"
  val BACKGROUND_YELLOW = "\u001B[43m"
  val BACKGROUND_BLUE = "\u001B[44m"
  val BACKGROUND_MAGENTA = "\u001B[45m"
  val BACKGROUND_CYAN = "\u001B[46m"
  val BACKGROUND_WHITE = "\u001B[47m"

  val HighIntensity = new Ansi(HIGH_INTENSITY)
  val LowIntensity = new Ansi(LOW_INTENSITY)

  val Italic = new Ansi(ITALIC)
  val Underline = new Ansi(UNDERLINE)
  val Blink = new Ansi(BLINK)
  val RapidBlink = new Ansi(RAPID_BLINK)

  val Black = new Ansi(BLACK)
  val Red = new Ansi(RED)
  val Green = new Ansi(GREEN)
  val Yellow = new Ansi(YELLOW)
  val Blue = new Ansi(BLUE)
  val Magenta = new Ansi(MAGENTA)
  val Cyan = new Ansi(CYAN)
  val White = new Ansi(WHITE)

  val BrightBlack = new Ansi(BRIGHT_BLACK)
  val BrightRed = new Ansi(BRIGHT_RED)
  val BrightGreen = new Ansi(BRIGHT_GREEN)
  val BrightYellow = new Ansi(BRIGHT_YELLOW)
  val BrightBlue = new Ansi(BRIGHT_BLUE)
  val BrightMagenta = new Ansi(BRIGHT_MAGENTA)
  val BrightCyan = new Ansi(BRIGHT_CYAN)
  val BrightWhite = new Ansi(BRIGHT_WHITE)

  val BgBlack = new Ansi(BACKGROUND_BLACK)
  val BgRed = new Ansi(BACKGROUND_RED)
  val BgGreen = new Ansi(BACKGROUND_GREEN)
  val BgYellow = new Ansi(BACKGROUND_YELLOW)
  val BgBlue = new Ansi(BACKGROUND_BLUE)
  val BgMagenta = new Ansi(BACKGROUND_MAGENTA)
  val BgCyan = new Ansi(BACKGROUND_CYAN)
  val BgWhite = new Ansi(BACKGROUND_WHITE)
}

class Ansi(val codes: String*) {
  private val codes_str = codes.mkString

  def and(other: Ansi): Ansi = {
    new Ansi(codes ++ other.codes: _*)
  }

  def + = and _ // Alias

  def apply(original: String) = {
    codes_str + original + Ansi.SANE
  }

  def colorize = apply _ // Alias

  def format(template: String, args: Any*) = {
    apply(template.format(args: _*))
  }

  def % = format _ // Alias
}