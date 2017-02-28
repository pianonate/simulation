/**
 * Created by nathan mccoy on 1/15/17.
 * used to test all combinations of specification entries
 */
import org.scalatest.FlatSpec

class TestSpecification extends FlatSpec {

  behavior of "A Specification"

  it must "for a spec with all optimization factors on, drive a working Game" in {

    new GameInfoFixture {

      val fullSpec = Specification(filtered=false)

      assert(fullSpec.spec.nonEmpty)
      context.stopGameAtRound = 2 // ensure the game runs fast - just to exercise code
      context.gamesToPlay = 1 // only run one game
      context.show = false // don't show results as the game is playing

      context.specification = fullSpec
      val game = new Game(context, multiGameStats)
      val result: GameResults = game.run
      assert(result.score > 0)
      assert(result.rounds > 0)

    }

  }

}
