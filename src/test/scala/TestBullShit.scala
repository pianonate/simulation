/**
 * Created by rhialtotm on 3/15/17.
 * oh yeah - test that bs
 */
class TestBullShit extends ContextSpec {

  trait BullShitFixture {
    val rounds = new Counter()
    val gameTimer = new GameTimer()
    val bs = new BullShit(rounds, gameTimer)

  }

  behavior of "the BullShit"

  it must "invoke the size method, which is a hack to make the debugger work" in {
    new BullShitFixture {
      val size = bs.size
      assert(size === 0)
    }
  }

  it must "return new bullshit" in {
    new BullShitFixture {
      assert(bs.iterator.hasNext)
      val s = bs.getNewBullShit
      assert(s.length > 0)

    }
  }

}
