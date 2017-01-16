/**
 * Created by nathanmccoy on 1/15/17.
 * used to test all combinations of specification entries
 */
import org.scalatest.{FlatSpec, _}
class TestSpecification extends FlatSpec {

  behavior of "A Specification"

  it must "for all combinations (and permuations of those combinations) drive a working Game" in {

    val allSpecs = Specification.getAllSpecifications
    assert(allSpecs.length > 0)

    val info = GameInfo(0, 0, 0, new GameTimer)
    var i = 0
    allSpecs.par.foreach { perm =>
      perm.par.foreach { spec =>

        i += 1

        // show something is happening as this whole test takes about 8seconds to run on the Mac Pro
        if (i % 100 ==0 ) println("spec #: " + i + " - length: " + spec.length + " factors: " + spec.map(s => s.fieldName).mkString(", "))

        val context = new Context(Array(), Specification(spec))

        context.maxSimulations = 1 // ensure the game runs super fast - just to exercise code
        context.continuousMode = false // only run one game
        context.show = false // don't show results as the game is playing

        val game = new Game(context, info) // get a valid game
        val result = game.run // run that game and get the score and number of rounds

        // todo return gameresults rather than a tuple - it will be easier to parse
        assert(result._1 > 0) // it just has to have a score
        assert(result._2 > 0) // it just has to have played some rounds

      }
    }

  }

}
