import org.scalatest.FlatSpec

/**
 * Created by rhialtotm on 3/10/17.
 * refactoring to be able to pass in the board size on the command line
 */

trait ContextFixture {
  val context = Context()
}

class TestContext extends FlatSpec {

  behavior of "the Context"

  it must "generate hash codes that add up in any combination of 2 or 3 to unique values" in {
    new ContextFixture {

      private val hashCodes: Array[Long] = context.allLocationHashes
      private val combo3 = hashCodes.combinations(3).toSet // guarantees uniqueness
      private val mappedCombo3 = combo3.map(_.sum)
      // after mapping to a new set by summing the hashCodes, if they weren't unique then the sizes would be different
      assert(combo3.size === mappedCombo3.size)

      private val combo2 = hashCodes.combinations(2).toSet
      private val mappedCombo2 = combo2.map(_.sum)
      assert(combo2.size === mappedCombo2.size)

      // this test ensures that the routine will work that allows us to skip
      // simulations when permutations generate the same
      // piece placements i.e., adding the hashCodes from any three locations generates a unique value
    }
  }
}
