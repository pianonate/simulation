/**
  * Created by nathan mccoy on 1/27/17.
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

case class PerformanceInfo(
                            simulatedCount:      Int,
                            unsimulatedCount:    Int,
                            rcChangedCountBest:  Int,
                            rcChangedCountWorst: Int,
                            elapsedMs:           Long,
                            perSecond:           Int
                          )


class GameStats {

  private var lastRoundStats:Option[PerformanceInfo] = None
  private val perSecondArray:Array[Int] = Array.fill[Int](100)(0)
  private var current = 0
  private var bestPerSecondAcc:Int = 0
  private var totalSimulationsAcc:Int = 0
  private var totalUnsimulatedAcc:Int = 0
  private var totalRaceConditionOnBestAcc:Int = 0
  private var totalRaceConditionOnWorstAcc:Int = 0

  def updateStats(perfInfo:PerformanceInfo):Unit = {
    lastRoundStats = Some(perfInfo)

    if (current == perSecondArray.length) current = 0

    perSecondArray(current) = perfInfo.perSecond
    current += 1

    if (perfInfo.perSecond > bestPerSecondAcc) bestPerSecondAcc = perfInfo.perSecond

    totalSimulationsAcc += perfInfo.simulatedCount + perfInfo.unsimulatedCount
    totalUnsimulatedAcc += perfInfo.unsimulatedCount
    totalRaceConditionOnBestAcc += perfInfo.rcChangedCountBest
    totalRaceConditionOnWorstAcc += perfInfo.rcChangedCountWorst

  }

  /*def simulatedCount = lastRoundStats.get.simulatedCount
  def unsimulatedCount = lastRoundStats.get.unsimulatedCount
  def rcChangedCountBest = lastRoundStats.get.rcChangedCountBest
  def rcChangedCountWorst = lastRoundStats.get.rcChangedCountWorst
  def elapsedMS = lastRoundStats.get.elapsedMs
  def perSecond = lastRoundStats.get.perSecond*/

  def lastRoundInfo: PerformanceInfo = lastRoundStats.get
  def averagePerSecond: Int = perSecondArray.avg.toInt
  def bestPerSecond: Int = bestPerSecondAcc
  def totalSimulations: Int = totalSimulationsAcc
  def totalUnsimulatedSimulations: Int = totalUnsimulatedAcc
  def totalRaceConditionOnBest: Int = totalRaceConditionOnBestAcc
  def totalRaceConditionOnWorst: Int = totalRaceConditionOnWorstAcc


}

