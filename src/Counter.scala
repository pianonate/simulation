/**
 * Created by nathan mccoy on 12/24/16.
 * encapsulate counter logic
 */
case class Counter(from: Int = 0) {

  private var counter: Int = from

  def value: Int = counter
  override def toString: String = value.toString

  def inc(): Unit = synchronized(counter += 1)
  def inc(count: Int): Unit = counter += count

}
