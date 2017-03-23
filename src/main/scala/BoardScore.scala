/**
 * Created by nathan on 1/30/17.
 * used for migration to weighted scoring system
 *
 */

case class FeatureScore(
  key:             String,
  intValue:        Int,
  normalizedValue: Double,
  weightedValue:   Double,
  weight:          Double
)

object FeatureScore {

  // case class constructor
  def apply(feature: Feature, intValue: Int): FeatureScore = {

    // normalize the value to range from 0 to 1.
    // if an optimization factor is supposed to a low number, then subtract it from it's max value before normalizing
    val normalizedValue: Double = if (feature.minimize)
      1 - (intValue - feature.minVal) / (feature.maxVal - feature.minVal).toDouble
    else
      (intValue - feature.minVal) / (feature.maxVal - feature.minVal).toDouble

    // todo - turn this assertion into test
    // assert(normalizedValue <= 1.0, opt.key + ".normalizedValue > 1.0 - here's the scoop: " + normalizedValue)

    // the weighted value is multiplied by the opt factor's weight - which in theory will guarantee goodness
    val weightedValue: Double = normalizedValue * feature.weight

    FeatureScore(feature.key, intValue, normalizedValue, weightedValue, feature.weight)

  }
}

case class BoardScore(
  board:   Board,
  context: Context
) {

  private val specification = context.specification

/*
  private def getScore(feature: Feature, intValue: Int): FeatureScore = {

    // normalize the value to range from 0 to 1.
    // if an optimization factor is supposed to a low number, then subtract it from it's max value before normalizing
    val normalizedValue: Double = if (feature.minimize)
      1 - (intValue - feature.minVal) / (feature.maxVal - feature.minVal).toDouble
    else
      (intValue - feature.minVal) / (feature.maxVal - feature.minVal).toDouble

   // assert(normalizedValue <= 1.0, opt.key + ".normalizedValue > 1.0 - here's the scoop: " + normalizedValue)

    // the weighted value is multiplied by the opt factor's weight - which in theory will guarantee goodness
    val weightedValue: Double = normalizedValue * feature.weight

    FeatureScore(feature.key, intValue, normalizedValue, weightedValue, feature.weight)

  }
*/

  private[this] val neighbors = board.countNeighbors(context.allLocations)

 /* private[this] val avoidMiddleScore: FeatureScore = FeatureScore(specification.avoidMiddleFeature, board.avoidMiddleSum)
  private[this] val fourNeighborsScore: FeatureScore = FeatureScore(specification.fourNeighborsFeature, neighbors(4))
  private[this] val lineContiguousScore: FeatureScore = FeatureScore(specification.spacesOnALineFeature, board.grid.lineContiguousCount)
  private[this] val contiguousOpenScore: FeatureScore = FeatureScore(specification.contiguousOpenFeature, board.grid.maxContiguousOpenLines)
  private[this] val maximizerScore: FeatureScore = FeatureScore(specification.maximizerFeature, board.maximizerCount)
  private[this] val occupiedScore: FeatureScore = FeatureScore(specification.occupiedFeature, board.grid.popCount)
  private[this] val openLinesScore: FeatureScore = FeatureScore(specification.openLinesFeature, board.grid.openLineCount)
  private[this] val roundScore: FeatureScore = FeatureScore(specification.roundScoreFeature, board.roundScore)
  private[this] val threeNeighborsScore: FeatureScore = FeatureScore(specification.threeNeighborsFeature, neighbors(3))
  private[this] val twoNeighborsScore: FeatureScore = FeatureScore(specification.twoNeighborsFeature, neighbors(2))*/

  // experimented with just creating an array and sorting it to match the spec
  // on MacPro profiler this generated 1,724 BoardScore.<init> /s
  // using the getNamedScore algo: 5,284/s
  // about 3x faster in the profiler
  val scores: Array[FeatureScore] = {

    def getNamedScore(name: String): FeatureScore = {

      name match {
        case Specification.avoidMiddleKey    => FeatureScore(specification.avoidMiddleFeature, board.avoidMiddleSum)
        case Specification.fourNeighborsKey  => FeatureScore(specification.fourNeighborsFeature, neighbors(4))
        case Specification.spacesOnALineKey  => FeatureScore(specification.spacesOnALineFeature, board.grid.lineContiguousCount)
        case Specification.occupiedKey       => FeatureScore(specification.occupiedFeature, board.grid.popCount)
        case Specification.contiguousOpenKey => FeatureScore(specification.contiguousOpenFeature, board.grid.maxContiguousOpenLines)
        case Specification.maximizerKey      => FeatureScore(specification.maximizerFeature, board.maximizerCount)
        case Specification.openLinesKey      => FeatureScore(specification.openLinesFeature, board.grid.openLineCount)
        case Specification.roundScoreKey     => FeatureScore(specification.roundScoreFeature, board.roundScore)
        case Specification.threeNeighborsKey => FeatureScore(specification.threeNeighborsFeature, neighbors(3))
        case Specification.twoNeighborsKey   => FeatureScore(specification.twoNeighborsFeature, neighbors(2))
        case _ =>
          throw new IllegalArgumentException("This optimization factor was requested but hasn't been added to the scores List: " + name)
      }
    }

    val factors = specification.featuresArray
    val scoreArray = new Array[FeatureScore](factors.length)

    var i = 0
    while (i < factors.length) {
      val factor = factors(i)
      scoreArray(i) = getNamedScore(factor.key)
      i += 1

    }

    scoreArray

  }

  // once you get the weights kicked in, this weightedSum
  // will be what you use to compare Simulations to each other
  // the compare will go _a lot_ faster when using this - basically the tuple compare goes away and now we're just comparing doubles
  val weightedSum: Double = {

    var i = 0
    var sum = 0.0
    while (i < scores.length) {
      sum += scores(i).weightedValue
      i += 1
    }

    // todo - turn this assertion into tests

/*    assert(sum <= 1.0, {
      val s = scores.map(score =>
        score.key + ", " +
          score.intValue + ", " +
          score.normalizedValue + ", " +
          score.weightedValue + ", " +
          score.weight).mkString("\n")
      throw new IllegalArgumentException("weighted sum exceeds 1.0 - here's the scoop:\n\n" + s + "\n\nsum:" + sum)
    })*/

    sum
  }
}

