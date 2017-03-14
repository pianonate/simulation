/**
 * Created by rhialtotm on 2/5/17.
 * test GameRunner class
 */

import org.scalatest.FlatSpec

class TestGameRunner extends FlatSpec {

  behavior of "A GameRunner"

  it must "run two games if two games are requested" in {

    val context = Context()

    context.gamesToPlay = 2
    context.stopGameAtRound = 1
    context.show = false

    val scores = GameRunner.play(context)

    assert(2 === scores.length, "requested 2 games, but got this many: " + scores.length)
  }

  it must "generate new random weights each game if not using fixed weights" in {
    // for this test to work, you need to run games the way they are run in GameRunner.play
    val context = Context()
    context.show = false
    context.gamesToPlay = 1
    context.stopGameAtRound = 1

    GameRunner.play(context)

    val game1Weight = context.specification.spec.head._2.weight

    GameRunner.play(context)

    val game2Weight = context.specification.spec.head._2.weight
    assert(game1Weight != game2Weight)

    GameRunner.play(context)
    val game3Weight = context.specification.spec.head._2.weight

    assert(game3Weight!=game1Weight && game3Weight != game2Weight)


  }

}
