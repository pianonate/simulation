/**
 * Created by nathan on 12/19/16.
 * used to calculate entropy for a sliding window of size n
 * against a 2D array of Booleans (indicating on/off)
 * adapted from
 * https://github.com/cosmoharrigan/matrix-entropy
 * which was an implementation of the discussion in
 * http://stats.stackexchange.com/questions/17109/measuring-entropy-information-patterns-of-a-2d-binary-matrix/17556#17556
 * just by the way, the scala code seems easier than the python...
 *
 */

object Entropy {

  def scaledEntropy(n: Int, matrix: Array[Array[Cell]]): Double = {

    def distributionAtLocation(row: Int, col: Int): Double = {
      val occupiedPositions = for {
        i <- row until (row + n)
        j <- col until (col + n)
        if matrix(i)(j).occupied
      } yield (i, j)

      // the distribution is the number of
      // occupied positions divided by the
      // size of the moving window
      occupiedPositions.length.toDouble / (n * n)

    }

    val rows = matrix.length - n
    val cols = matrix.length - n

    // create an Array[Array[Double]] for the distribution of Booleans in a sliding window of size n
    // against the passed in Matrix
    val a = for {
      i <- 0 to rows
      j <- 0 to cols
    } yield distributionAtLocation(i, j)

    // the original implementation created a new array out of the sliding window and then subsequently flattened it
    // i'm skipping that and just returning the flattened values to start
    matrixEntropy(a.toList)

  }

  private def matrixEntropy(a: List[Double]): Double = {

    // we're trying to get a distribution of resultant values from the distribution at each sliding window entry
    // so group by all the members of the flattened matrix to find out how often a number repeats
    val counts = a.groupBy(l => l).map(t => t._2.length.toDouble)

    // the denominator is the total numbers in the original set - so just sum the lengths of the distribution from the previous step
    val totalCount = counts.sum

    // the discrete distribution is the counts divided by the total count
    // so if a window of 3 had all of its positions on then it would be 9/9 and we'd have a 1.0 in the list
    // we want the number of times out of the total count of windows that the number 1.0 shows up (and all the other counts that show up)
    val discreteDist = counts.map(_ / totalCount).toList

    // find the entropy of the list of discrete distributions of values in the original matrix
    entropy(discreteDist)

  }

  private def entropy(probabilityList: List[Double]): Double = {

    def log2(x: Double) = scala.math.log(x) / scala.math.log(2)

    probabilityList.map(item => item * log2(item)).sum * -1.0

  }

}
