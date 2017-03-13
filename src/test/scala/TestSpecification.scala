/**
 * Created by rhialtotm on 1/15/17.
 * used to test all combinations of specification entries
 */
import org.scalatest.FlatSpec

class TestSpecification extends FlatSpec {

  behavior of "A Specification"

  it must "drive a working game from a specification" in {

    new GameInfoFixture {

      private val spec = context.specification

      assert(spec.spec.nonEmpty)
      context.stopGameAtRound = 2 // ensure the game runs fast - just to exercise code
      context.gamesToPlay = 1 // only run one game
      context.show = false // don't show results as the game is playing

      val game = new Game(context, multiGameStats)
      val result: GameResults = game.run
      assert(result.score > 0)
      assert(result.rounds > 0)

    }
  }

    it must "use fixed weights if using the fixed weights command line argument" in {

      // the specification is randomized by copying the allFeatures listMap and randomizing weights
      // if the fixed weight argument is used, then allFeatures spec is used "as is"
      val context = Context(Array("-" + Conf.fixedWeightArg.toString))
      val specification = context.specification
      // get the weight that was constructed based on the command line argument
      val specMaximizerWeight = specification.spec(Specification.maximizerKey).weight

      // get the fixed weight by constructing it yourself
      // this is sort of a gimme as we expose allFeatures and weightedSpecification publicly just so we can construct
      // it in this test the way we expect it to be constructed at run time
      // this is mostly just making sure you don't f-up the fixed weight arg
      val fixedWeightedSpec = specification.weightedSpecification(specification.allFeatures)
      val fixedMaximizerWeight = fixedWeightedSpec(Specification.maximizerKey).weight

      // this test tells us the fixed weight comand line argument works
      assert(specMaximizerWeight===fixedMaximizerWeight)
    }


}
