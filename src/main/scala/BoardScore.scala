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

case class BoardScore(
  board:   Board,
  context: Context
) {

  private val specification = context.specification

  // sum the weighted results and use this as an alternative comparison to current mechanism

  private def getScore(opt: Feature, intValue: Int): FeatureScore = {

    // normalize the value to range from 0 to 1.
    // if an optimization factor is supposed to a low number, then subtract it from it's max value before normalizing
    val normalizedValue: Double = if (opt.minimize)
      1 - (intValue - opt.minVal) / (opt.maxVal - opt.minVal).toDouble
    else
      (intValue - opt.minVal) / (opt.maxVal - opt.minVal).toDouble

    assert(normalizedValue <= 1.0, opt.key + ".normalizedValue > 1.0 - here's the scoop: " + normalizedValue)

    // the weighted value is multiplied by the opt factor's weight - which in theory will guarantee goodness
    val weightedValue: Double = normalizedValue * opt.weight

    FeatureScore(opt.key, intValue, normalizedValue, weightedValue, opt.weight)

  }

  private val neighbors = board.countNeighbors(context.allLocations)

  val avoidMiddleScore: FeatureScore = getScore(specification.avoidMiddleFeature, board.avoidMiddleSum)
  val fourNeighborsScore: FeatureScore = getScore(specification.fourNeighborsFeature, neighbors(4))
  val lineContiguousScore: FeatureScore = getScore(specification.spacesOnALineFeature, board.grid.lineContiguousCount)
  val contiguousOpenScore: FeatureScore = getScore(specification.contiguousOpenFeature, board.grid.maxContiguousOpenLines)
  val maximizerScore: FeatureScore = getScore(specification.maximizerFeature, board.maximizerCount)
  val occupiedScore: FeatureScore = getScore(specification.occupiedFeature, board.grid.popCount)
  val openLinesScore: FeatureScore = getScore(specification.openLinesFeature, board.grid.openLineCount)
  val roundScore: FeatureScore = getScore(specification.roundScoreFeature, board.roundScore)
  val threeNeighborsScore: FeatureScore = getScore(specification.threeNeighborsFeature, neighbors(3))
  val twoNeighborsScore: FeatureScore = getScore(specification.twoNeighborsFeature, neighbors(2))

  // experimented with just creating an array and sorting it to match the spec
  // on MacPro profiler this generated 1,724 BoardScore.<init> /s
  // using the getNamedScore algo: 5,284/s
  // aboux 3x faster in the profiler
  val scores: Array[FeatureScore] = {

    def getNamedScore(name: String): FeatureScore = {

      name match {
        case Specification.avoidMiddleKey    => avoidMiddleScore
        case Specification.fourNeighborsKey  => fourNeighborsScore
        case Specification.spacesOnALineKey  => lineContiguousScore
        case Specification.occupiedKey       => occupiedScore
        case Specification.contiguousOpenKey => contiguousOpenScore
        case Specification.maximizerKey      => maximizerScore
        case Specification.openLinesKey      => openLinesScore
        case Specification.roundScoreKey     => roundScore
        case Specification.threeNeighborsKey => threeNeighborsScore
        case Specification.twoNeighborsKey   => twoNeighborsScore
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

    assert(sum <= 1.0, {
      val s = scores.map(score =>
        score.key + ", " +
          score.intValue + ", " +
          score.normalizedValue + ", " +
          score.weightedValue + ", " +
          score.weight).mkString("\n")
      throw new IllegalArgumentException("weighted sum exceeds 1.0 - here's the scoop:\n\n" + s + "\n\nsum:" + sum)
    })

    sum
  }
}

