/**
 * Created by nathan mccoy on 1/15/17.
 * used to test all combinations of specification entries
 */
import org.scalatest.FlatSpec

class TestSpecification extends FlatSpec {

  behavior of "A Specification"

  it must "for all optimization factor, drive a working Game" in {

    new GameInfoFixture {

      val fullSpec: scala.collection.immutable.ListMap[String,OptimizationFactor] = Specification.fullSpecification
      assert(fullSpec.nonEmpty)
      context.maxSimulations = 10 // ensure the game runs super fast - just to exercise code
      context.continuousMode = false // only run one game
      context.show = false // don't show results as the game is playing

      context.specification = Specification(fullSpec)
      val game = new Game(context, multiGameStats)
      val result: GameResults = game.run
      assert(result.score > 0)
      assert(result.rounds > 0)

    }
  }

}
