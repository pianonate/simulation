/**
 * Created by rhialtotm on 1/27/17.
 * added game stats to speed up processing when getting round results
 * prior to this PerformanceInfo was stored in a ListBuffer
 * LisBuffer was used to get max simulations per second as well as
 * average over the last 100
 * when the game starts, getting round results was fast
 * but as the game progressed, the ListBuffer grew and
 * getting the round results could grow to take nearly 10%
 * of the overall execution time of the program - as measured by a GameTimer
 * by moving PerformanceInfo into mutable state, round results
 * now take approximately .05% of the program execution time or less - consistently
 */
import Implicits._

case class MultiGameStats(
  averageScore:     Int,
  sessionHighScore: Int,
  machineHighScore: Int,
  gameNumber:       Int,
  totalTime:        GameTimer
)

case class PerformanceInfo(
  simulatedCount:     Int,
  unsimulatedCount:   Int,
  rcChangedCountBest: Int,
  elapsedMs:          Long,
  perSecond:          Int
)

class GameStats {

  private var lastRoundStats: Option[PerformanceInfo] = None
  private val perSecondArray: Array[Int] = Array.fill[Int](100)(0)
  private var current = 0
  private var bestPerSecondAcc: Int = 0
  private var totalSimulationsAcc: Int = 0
  private var totalUnsimulatedAcc: Int = 0
  private var totalRaceConditionOnBestAcc: Int = 0

  def updateStats(perfInfo: PerformanceInfo): Unit = {
    lastRoundStats = Some(perfInfo)

    if (current == perSecondArray.length) current = 0

    perSecondArray(current) = perfInfo.perSecond
    current += 1

    if (perfInfo.perSecond > bestPerSecondAcc) bestPerSecondAcc = perfInfo.perSecond

    totalSimulationsAcc += perfInfo.simulatedCount + perfInfo.unsimulatedCount
    totalUnsimulatedAcc += perfInfo.unsimulatedCount
    totalRaceConditionOnBestAcc += perfInfo.rcChangedCountBest

  }

  def lastRoundInfo: PerformanceInfo = lastRoundStats.get
  def averagePerSecond: Int = perSecondArray.avg.toInt
  def bestPerSecond: Int = bestPerSecondAcc
  def totalSimulations: Int = totalSimulationsAcc
  def totalUnsimulatedSimulations: Int = totalUnsimulatedAcc
  def totalRaceConditionOnBest: Int = totalRaceConditionOnBestAcc

}

