/**
 * Created by nathan mccoy on 1/15/17.
 * used to test all combinations of specification entries
 */
import org.scalatest.{FlatSpec, _}

class TestSpecification extends FlatSpec {

  behavior of "A Specification"

  it must "for all optimization factor, drive a working Game" in {

    new GameInfoFixture {

      val fullSpec: Array[OptimizationFactor] = Specification.fullSpecification
      assert(fullSpec.length > 0)
      context.maxSimulations = 10 // ensure the game runs super fast - just to exercise code
      context.continuousMode = false // only run one game
      context.show = false // don't show results as the game is playing

      context.specification = Specification(fullSpec)
      val game = new Game(context, gameInfo)
      val result: GameResults = game.run
      assert(result.score > 0)
      assert(result.rounds > 0)

      /*      var i = 0
      allSpecs.foreach { perm =>
        perm.par.foreach { spec =>

          i += 1

          // show something is happening as this whole test takes about 8seconds to run on the Mac Pro
          if (i % 100 == 0) println("spec #: " + i + " - length: " + spec.length + " factors: " + spec.map(s => s.fieldName).mkString(", "))



          // override some defaults
          context.specification = Specification(spec)

          val game = new Game(context, gameInfo) // get a valid game

          val result = game.run // run that game and get the score and number of rounds

          // todo return game results rather than a tuple - it will be easier to parse
          assert(result._1 > 0) // it just has to have a score
          assert(result._2 > 0) // it just has to have played some rounds

        }
      }*/
    }
  }

}
