/**
 * Created by nathan on 12/18/16.
 * keep track of important durations
 * Timer's are single use.  Once you stop the timer, it is stopped for good
 * Timer is stopped by invoking elapsed or calling toString (which calls elapsed)
 */
class GameTimer {

  private val t0 = System.currentTimeMillis()

  // so this has a funky use
  // any time you call elapsed or toString (which calls elapsed)
  // you stop the clock because you'll lazy evaluate the t1 field...
  // don't think this is a super great way to go, but it works for the current usages
  def elapsed: Long = System.currentTimeMillis() - t0

  def showElapsed: String = {
    val now = elapsed
    val hour: Long = 60 * 60 * 1000
    val minute: Long = 60 * 1000
    val second: Long = 1000

    val hours = math.floor(now / hour).toLong
    val minutes = math.floor((now - (hours * hour)) / minute).toLong
    val seconds = math.floor((now - ((hours * hour) + (minutes * minute))) / second).toLong
    val millis = now - ((hours * hour) + (minutes * minute) + (second * seconds)).toLong

    // 3h 4m 53s 42ms
    val twoDigits = "%2d"
    val threeDigits = "%3d"


    val s: String = (
      (if (hours > 0) hours.toString + "h " else "")
      + twoDigits.format(minutes) + "m "
      + twoDigits.format(seconds) + "s "
      + threeDigits.format(millis) + "ms"
      //+ " (" + Game.numberFormat.format(now) + ")"
    )

    s
  }

}
