/**
 * Created by nathan on 12/18/16.
 * keep track of important durations
 * Timer's are single use.  Once you stop the timer, it is stopped for good
 * Timer is stopped by invoking elapsed or calling toString (which calls elapsed)
 */
class Timer {

  private val t0 = System.currentTimeMillis()
  private lazy val t1 = System.currentTimeMillis()

  // so this has a funky use
  // any time you call elapsed or toString (which calls elapsed)
  // you stop the clock because you'll lazy evaluate the t1 field...
  // don't think this is a super great way to go, but it works for the current usages
  def elapsed: Long = t1 - t0

  def showElapsed: String = {
    val now = elapsed
    val hour: Long = 60 * 60 * 1000
    val minute: Long = 60 * 1000
    val second: Long = 1000

    val hours = now / hour
    val minutes = (now - (hours * hour)) / minute
    val seconds = (now - ((hours * hour) + (minutes * minute))) / minute
    val millis = now - ((hours * hour) + (minutes * minute) + (seconds * second))

    // 3h 4m 53s 42ms
    val s: String = (
      (if (hours > 0) hours.toString + "h " else "")
      + minutes.toString + "m "
      + seconds.toString + "s "
      + millis.toString + "ms"
    )

    s
  }

}
