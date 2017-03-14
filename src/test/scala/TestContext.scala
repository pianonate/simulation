/**
 * Created by rhialtotm on 3/10/17.
 * refactoring to be able to pass in the board size on the command line
 */


class TestContext extends ContextSpec {

  behavior of "the Context"

  it must "generate hash codes that add up in any combination of 2 or 3 to unique values" in {

    val context = getContext()
    val hashCodes: Array[Long] = context.allLocationHashes
    val combo3 = hashCodes.combinations(3).toSet // guarantees uniqueness
    val mappedCombo3 = combo3.map(_.sum)
    // after mapping to a new set by summing the hashCodes, if they weren't unique then the sizes would be different
    assert(combo3.size === mappedCombo3.size)

    val combo2 = hashCodes.combinations(2).toSet
    val mappedCombo2 = combo2.map(_.sum)
    assert(combo2.size === mappedCombo2.size)

    // this test ensures that the routine will work that allows us to skip
    // simulations when permutations generate the same
    // piece placements i.e., adding the hashCodes from any three locations generates a unique value

  }

  it must "have the same random weights from two different games if there is a gameSeed provided" in {
    val context = getContext(gameSeedArg)
    val multiGameStats = MultiGameStats(0, 0, 0, 1, new GameTimer)

    val game1 = new Game(context, multiGameStats)
    val weights1 = context.specification.spec.values.map(_.weight)

    val game2 = new Game(context, multiGameStats)
    val weights2 = context.specification.spec.values.map(_.weight)

    assert(weights1 === weights2)
  }

  it must "have different weights from two different games if there is no gameSeed provided" in {

    val context = getContext()

    val multiGameStats = MultiGameStats(0, 0, 0, 1, new GameTimer)

    val game1 = new Game(context, multiGameStats)
    val weights1 = context.specification.spec.values.map(_.weight)

    val game2 = new Game(context, multiGameStats)
    val weights2 = context.specification.spec.values.map(_.weight)

    assert(weights1 != weights2)
  }

  it must "return the gameSeed if both a gameSeed and a sessionSeed are passed on the command line" in {
    val args = Array(gameSeedArg, sessionSeedArg)
    val context = getContext(args)

    // call game pieces twice and ensure
    val series = Array.fill[Int](2)(context.getGamePieces.seed)
    assert(series.head === series.tail(0))

  }

  it must "return the same series of seeds when a multi-game seed is requested" in {

    val context1 = getContext(sessionSeedArg)
    val series1 = Array.fill[Int](5)(context1.getGamePieces.seed)

    val context2 = getContext(sessionSeedArg)
    val series2 = Array.fill[Int](5)(context2.getGamePieces.seed)

    assert(series1.sum === series2.sum)

  }

  it must "return a random game seed for each call to getGamePieces when no command line seed value is provided" in {

    val context = getContext()
    val gamePieces1 = context.getGamePieces
    val gameSeed1 = gamePieces1.seed

    val gamePieces2 = context.getGamePieces
    val gameSeed2 = gamePieces2.seed

    assert(gameSeed1 != gameSeed2)

  }

  it must "return the same game seed each time if game seed is passed in on the command line" in {
    val context = getContext(gameSeedArg)
    val gamePieces1 = context.getGamePieces
    val gameSeed1 = gamePieces1.seed

    val gamePieces2 = context.getGamePieces
    val gameSeed2 = gamePieces2.seed
    assert(gameSeed1 === gameSeed2)
  }

}
