/**
 * Created by nathan mccoy on 12/24/16.
 * encapsulate counter logic
 */
class Counter {

  // you could also use Stream.from(0
  private def intIterator(): Iterator[Int] = intIteratorFrom(0)

  // creates an infinite iterator on a long value starting at the given and incrementing by 1
  // you could also implement a stepBy if you wanted...
  private def intIteratorFrom(start: Int): Iterator[Int] = Iterator.iterate(start)(num => num + 1)

  private val iterator = intIterator().buffered

  def inc: Int = iterator.next()

  override def toString: String = value.toString

  def value: Int = iterator.head

  def incrementMultiple(count: Int): Int = { for (_ <- 0 until count) iterator.next; iterator.head }

}

object Counter {
  def apply(): Counter = new Counter()
}