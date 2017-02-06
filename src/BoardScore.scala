/**
 * Created by nathan on 1/30/17.
 * used for migration to weighted scoring system
 *
 */

case class ScoreComponent(
  label:           String,
  intValue:        Int,
  normalizedValue: Double,
  weightedValue:   Double,
  weight:          Double
)

case class BoardScore(
  board:         Board,
  specification: Specification
) {

  // sum the weighted results and use this as an alternative comparison to current mechanism

  private def getScore(opt: OptimizationFactor, i: Int): ScoreComponent = {

    if (opt.enabled) {

      // todo - when we stop using the compare, we don't have to multiply these by -1 anymore
      val intValue = if (opt.minimize) i else i * -1

      // normalize the value to be from 0 to 1.  as currently all min vals are 0, there is no need to include a
      // min val in the normalization
      // if an optimization factor is supposed to a low number, then subtract it from it's max value before normalizing
      val normalizedValue: Double = if (opt.minimize) 1 - (i - opt.minVal) / (opt.maxVal - opt.minVal).toDouble else (i - opt.minVal) / (opt.maxVal - opt.minVal).toDouble

      // the weighted value is multiplied by the opt factor's weight - which in theory will guarantee goodness
      val weightedValue: Double = normalizedValue * opt.weight

      ScoreComponent(opt.label, intValue, normalizedValue, weightedValue, opt.weight)
    } else
      ScoreComponent(opt.label + "- disabled", 0, 0.0, 0.0, opt.weight)

  }

  private val neighbors = board.countNeighbors(Board.allLocations)

  // todo it would be far more scalable to just map popCount to the optimization factor and have it call directly
  // but this would introduce a lambda which, at runtime, has historically slowed things down significantly
  //  - test this theory and see if we can go direct as this would make the code much more maintainable

  val avoidMiddleScore: ScoreComponent = getScore(specification.avoidMiddleOptFactor, board.avoidMiddleSum)
  val occupiedScore: ScoreComponent = getScore(specification.occupiedOptFactor, board.grid.popCount)
  val maximizerScore: ScoreComponent = getScore(specification.maximizerOptFactor, board.maximizerCount)
  // this is not how you would count it, you need to run a simulation at the end of the simulation
  // that would short circuit once it found the first working solution for each of the combinations of all maximizers
  // this would tell us that we have a winning
  ///val allMaximizersScore: ScoreComponent = getScore(specification.allMaximizersOptFactor, ???)
  val fourNeighborsScore: ScoreComponent = getScore(specification.fourNeighborsOptFactor, neighbors(4))
  val threeNeighborsScore: ScoreComponent = getScore(specification.threeNeighborOptFactor, neighbors(3))
  val twoNeighborsScore: ScoreComponent = getScore(specification.twoNeighborsOptFactor, neighbors(2))
  val maxContiguousLinesScore: ScoreComponent = getScore(specification.maxContiguousLinesOptFactor, board.grid.maxContiguousOpenLines)
  val openLinesScore: ScoreComponent = getScore(specification.openLinesOptFactor, board.grid.openLineCount)

  lazy val scores: Array[ScoreComponent] = {

    specification.spec.map {
      case (name, _) =>
        name match {
          case Specification.occupiedKey  => occupiedScore
          case Specification.maximizerKey => maximizerScore
          // case Specification.allMaximizersCountName => allMaximizersScore
          case Specification.avoidMiddleKey => avoidMiddleScore
          case Specification.`fourNeighborsKey`  => fourNeighborsScore
          case Specification.threeNeighborsKey => threeNeighborsScore
          case Specification.twoNeighborsKey   => twoNeighborsScore
          case Specification.maxContiguousKey  => maxContiguousLinesScore
          case Specification.openLinesKey      => openLinesScore
          case _ =>
            throw new IllegalArgumentException("This optimization factor was requested but hasn't been added to the scores List: " + name)
        }
    }.toArray

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

    if (sum > 1.0) {
      val s = scores.map(score =>
        score.label + ", " +
          score.intValue + ", " +
          score.normalizedValue + ", " +
          score.weightedValue + ", " +
          score.weight).mkString("\n")
      throw new IllegalArgumentException("weighted sum exceeds 1.0 - here's the scoop:\n\n" + s + "\n\nsum:" + sum)
    }

    sum
  }
}