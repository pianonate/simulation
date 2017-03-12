/**
 * Created by rhialtotm on 12/24/16.
 * encapsulate counter logic
 */
case class Counter(from: Int = 0) {

  private var counter: Int = from

  def value: Int = counter
  override def toString: String = value.toString
  // this can be accessed from multiple threads so protect it
  // returned value can be used for simulation ID to avoid calling .hashcode
  // which takes up 7% of execution time right now
  def inc(): Int = synchronized { counter += 1; counter }
  def inc(count: Int): Int = { counter += count; counter }

}
