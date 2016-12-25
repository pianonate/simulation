/**
 * Created by nathan mccoy on 12/24/16.
 * encapsulate counter logic
 */
class Counter {

  // you could also use Stream.from(0
  private def intIter(): Iterator[Int] = intIterFrom(0)

  // creates an infinite iterator on a long value starting at the given and incrementing by 1
  // you could also implement a stepBy if you wanted...
  private def intIterFrom(start: Int): Iterator[Int] = Iterator.iterate(start)(num => num + 1)

  private val iter = intIter.buffered

  def inc: Int = iter.next()

  override def toString = value.toString

  def value: Int = iter.head

  def incrementMultiple(count: Int): Int = { for (i <- 0l until count) iter.next; iter.head }

}

object Counter {
  def apply(): Counter = new Counter()
}