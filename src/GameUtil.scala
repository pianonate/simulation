/**
 * Created by nathan on 12/14/16.
 * used for a small number of support methods
 */
object GameUtil {

  // you could also use Stream.from(0
  def longIter:Iterator[Long] = longIterFrom(0)

  // creates an infinite iterator on a long value starting at the given and incrementing by 1
  // you could also implement a stepBy if you wanted...
  def longIterFrom(start:Long):Iterator[Long] = Iterator.iterate[Long](start)(l => l + 1)

}

