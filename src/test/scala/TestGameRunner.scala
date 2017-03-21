import org.scalatest.Matchers

/**
 * Created by rhialtotm on 2/5/17.
 * test GameRunner class
 */

class TestGameRunner extends ContextSpec {

  trait GameRunnerFixture {
    val gameRunner = new GameRunner with MockOutput
    gameRunner.testOnlySetMockOutput
  }

  behavior of "A GameRunner"

  it must "execute new high score code" in {
    new GameRunnerFixture {

      gameRunner.testOnlySimulateNewHighScore

      val context = getContext()
      context.gamesToPlay = 1
      context.stopGameAtRound = 1
      gameRunner.play(context)
      assert(gameRunner.messages(1).contains("new high score"))

    }
  }

  it must "run two games if two games are requested" in {
    new GameRunnerFixture {
      val context = getContext()

      context.gamesToPlay = 2
      context.stopGameAtRound = 1

      val scores = gameRunner.play(context)

      assert(2 === scores.length, "requested 2 games, but got this many: " + scores.length)
    }
  }

  it must "generate new random weights each game if not using fixed weights" in {
    new GameRunnerFixture {
      // for this test to work, you need to run games the way they are run in GameRunner.play
      val context = getContext()
      context.gamesToPlay = 1
      context.stopGameAtRound = 1

      gameRunner.play(context)

      val game1Weight = context.specification.spec.head._2.weight

      gameRunner.play(context)

      val game2Weight = context.specification.spec.head._2.weight
      assert(game1Weight != game2Weight)

      gameRunner.play(context)
      val game3Weight = context.specification.spec.head._2.weight

      assert(game3Weight != game1Weight && game3Weight != game2Weight)

    }
  }
}
