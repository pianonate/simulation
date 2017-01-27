/**
 * Created by nathan on 12/18/16.
 * keep track of important durations
 * Timer's are single use.  Once you stop the timer, it is stopped for good
 * Timer is stopped by invoking elapsed or calling toString (which calls elapsed)
 */

import GameTimer._
import Implicits._

class GameTimer {

  private def getTime =   System.nanoTime() // System.currentTimeMillis()

  private val startTime = getTime
  private var pausedTime = 0l
  private var resumedTime = 0l
  private var pausedDuration = 0l

  def pause: Unit =  { pausedTime = getTime;require(pausedTime>startTime) }
  def resume: Unit =  { resumedTime = getTime; require(resumedTime>pausedTime); pausedDuration += (resumedTime - pausedTime) }

  // so this has a funky use
  // any time you call elapsed or toString (which calls elapsed)
  // you stop the clock because you'll lazy evaluate the t1 field...
  // don't think this is a super great way to go, but it works for the current usages
  def elapsedNanoseconds: Long = {
    val duration = /*System.currentTimeMillis() */  getTime - (startTime + pausedDuration)
    // if it happens within the same millisecond, then to
    // avoid divide by zeros, return a minimum duration of 1 ms
    if (duration == 0) 1 else duration
  }


  def elapsedHours: Float = (elapsedNanoseconds.toDouble / hour).toFloat
  def elapsedMinutes: Float = (elapsedNanoseconds.toDouble / minute).toFloat
  def elapsedSeconds: Float = (elapsedNanoseconds.toDouble / second).toFloat
  def elapsedMilliseconds:Float =(elapsedNanoseconds.toDouble / millisecond).toFloat

  def elapsedHoursFloor: Int = math.floor(elapsedHours).toInt
  def elapsedMinutesFloor: Int = math.floor(elapsedMinutes).toInt
  def elapsedSecondsFloor: Int = math.floor(elapsedSeconds).toInt
  def elapsedMillisecondsFloor:Long = math.floor(elapsedMilliseconds).toLong

  def minutesThisHour: Int = elapsedMinutesFloor - (elapsedHoursFloor * babylonianBase)

  def secondsThisMinute: Int = elapsedSecondsFloor -
    ((elapsedHoursFloor * babylonianBase * babylonianBase) +
      (minutesThisHour * babylonianBase))

  def millisThisSecond: Int =  (elapsedMillisecondsFloor -
    ((elapsedHoursFloor * babylonianBase * babylonianBase) +
    (minutesThisHour * babylonianBase) + secondsThisMinute) * millisecondsPerSecond ).toInt

  private def getElapsed: String = {

    // 3h 4m 53s
    val s: String = (
      (if (elapsedHoursFloor > 0) elapsedHoursFloor.shortLabel + "h " else "")
      + (if (elapsedHoursFloor > 0 || minutesThisHour > 0) minutesThisHour.label(2) + "m " else "")
      + secondsThisMinute.label(2) + "s "
    )

    s
  }

  def showElapsed: String = {
    getElapsed
  }

  def showElapsedMs: String = {
    // 3h 4m 53s 42ms
    getElapsed + millisThisSecond.label(3) + "ms"
  }

}

object GameTimer {
  private val babylonianBase = 60
  val nanosecond:Long = 1000000 // switch to 1 for System.currentTimerMs()
  val millisecond: Long = 1 * nanosecond
  val second: Long = 1000 * millisecond
  val minute: Long = 60 * second
  val hour: Long = 60 * minute
  val millisecondsPerSecond:Long = 1000



}
