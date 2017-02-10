/**
 * Created by nathan mccoy on 2/5/17.
 */

import org.scalatest.FlatSpec

class TestGameRunner extends FlatSpec {

  behavior of "A GameRunner"

  it must "run two games if two games are requested" in {

    val context = new Context(new Conf(Seq()))
    context.gamesToPlay = 2
    context.stopGameAtRound = 1
    context.show = false

    val scores = GameRunner.play(context)

    assert(2===scores.length, "requested 2 games, but got this many: " + scores.length)
  }

}
