/**
  * Created by rhialtotm on 3/14/17.
  * Counter tests
  */

import org.scalatest.FlatSpec

class TestCounter extends FlatSpec {

  behavior of "A Counter"

  it must "start at 0" in {
    val c = Counter()
    assert (c.value === 0)
  }

  it must "allow arbitrary starting point" in {
    val c = Counter(3)
    assert(c.value === 3)
  }

  it must "increment by 1" in {
    val c = Counter()
    c.inc()
    assert(c.value === 1)
  }

  it must "increment by arbitrary amount" in {
    val c = Counter()
    c.inc(5)
    assert(c.value === 5)
  }

  it must "generate proper toString result" in {
    val c = Counter()
    val s = c.toString
    assert(s === "0")
  }

}
