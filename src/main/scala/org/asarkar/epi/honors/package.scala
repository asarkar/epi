package org.asarkar.epi

import scala.annotation.tailrec

package object honors {
  /*
   * 24.2 Given an array of integers, find the first missing positive integer in linear time and constant space.
   * In other words, find the lowest positive integer that does not exist in the array.
   * The array can contain duplicates and negative numbers as well.
   * For example, the input [3, 4, -1, 1] should give 2. The input [1, 2, 0] should give 3.
   * You can modify the input array in-place.
   *
   * ANSWER: We make a crucial observation: if the input array is partitioned such that positive numbers precede
   * the negative numbers, the missing positive number is an integer between 1 and n + 1, where n is the index
   * of the last positive number. This is because if the array had all integers from 1 to n, then the missing integer
   * would be n + 1. We use a modified 3-way partitioning algorithm for the partitioning step.
   *
   * We need some way to mark the integers that are present. If we were allowed to used O(n) space, we could have a
   * boolean array of length n + 1, and set the indices corresponding to the values found in the input array to true.
   * Since we are not allowed to use additional space, we will modify the input array to indicate which integers are
   * present. We do so by walking the array, and for 0 < i <= n, we set A[i - 1] = -A[i - 1]. We then walk the array,
   * again and when we find a positive value, return its index + 1 as the answer.
   */
  def firstMissingPositiveNumber(xs: Array[Int]): Int = {
    def swap(i: Int, j: Int): Unit = {
      if (i != j) {
        val tmp = xs(i)
        xs(i) = xs(j)
        xs(j) = tmp
      }
    }

    var hi = 0
    var mid = 0
    var lo = xs.length - 1
    val pivot = 0

    while (mid <= lo) {
      if (xs(mid) > pivot) {
        swap(mid, hi)
        mid += 1
        hi += 1
      } else if (xs(mid) < pivot) {
        swap(mid, lo)
        // don't increment mid yet since we don't know anything about the element that ended up there
        lo -= 1
      } else mid += 1
    }

    // if xs(lo) < 0, there are no positive numbers in the array, otherwise lo points to the last non-negative number
    assert(!(0 until lo).exists(xs(_) < pivot),
      s"All elements on the left of index: $lo must be >= $pivot, ${xs.deep}"
    )
    assert(!(hi + 1 until xs.length).exists(xs(_) > pivot),
      s"All elements on the right of index: $hi must be <= $pivot, ${xs.deep}"
    )

    (0 to lo)
      .filter(i => xs(i) > 0)
      .foreach { i =>
        val x = math.abs(xs(i))
        // may already be negative if there're duplicates in the array
        if (xs(x - 1) > 0) xs(x - 1) *= -1
      }

    (0 to lo)
      .find(i => i >= 0 && xs(i) >= 0)
      .getOrElse(0) + 1
  }

   /*
    * 24.32 You are given an array of non-negative integers that represents a two-dimensional elevation map where each element
    * is unit-width wall and the integer is the height. Suppose it will rain and all spots between two walls get filled
    * up.
    * Compute how many units of water remain trapped on the map in O(N) time and O(1) space.
    *
    * For example, given the input [2, 1, 2], we can hold 1 unit of water in the middle.
    * Given the input [3, 0, 1, 3, 0, 5], we can hold 3 units in the first index, 2 in the second, and 3 in the fourth
    * index (we cannot hold 5 since it would run off to the left), so we can trap 8 units of water.
    *
    * ANSWER: See rainwater.png for a visual explanation of the problem. We make a crucial observation that the amount
    * of rain water trapped between two buildings (can equivalently be thought of as water trapped on top of a building)
    * depends on the minimum of the maximum heights on either side of the building. Any more than the height of the
    * minimum, and the water will run off. The amount of water trapped is the difference between the heights of the min
    * height building and the one in question.
    * A naive way to solve this problem would be to find the building with the max height on either side of a building.
    * That, however, yields a O(n^2) algorithm. Can we do better?
    *
    * What if we start moving inward from the boundaries, keeping track of the respective maximum heights? If the current
    * building on the left is shorter than the one on the right, the amount of water trapped depends only on the
    * building with the max height on the left. This is perhaps not very intuitive, but it is true for all elevation
    * maps.
    *
    * Similarly, if a building is shorter than the one on its left, the amount of water trapped on it depends solely on
    * the building with the max height on its right.
    *
    * If a building is taller than the one on its left or right, we can't tell anything about the amount of water trapped
    * on it.
    *
    * The following code follows from the explanation above.
    */
  def rainWaterTrapped(xs: IndexedSeq[Int]): Int = {
    def loop(l: Int, r: Int, lMax: Int, rMax: Int): Int = {
      if (l <= r) {
        val left = xs(l)
        val right = xs(r)
        if (left < right) {
          val max = math.max(lMax, left)
          (max - left) + loop(l + 1, r, max, rMax)
        } else {
          val max = math.max(rMax, right)
          (max - right) + loop(l, r - 1, lMax, max)
        }
      } else 0
    }

    loop(0, xs.size - 1, -1, -1)
  }

  /*
   * 24.36 Suppose you are given a table of currency exchange rates, represented as a 2D array. Determine whether there is a
   * possible arbitrage: that is, whether there is some sequence of trades you can make, starting with some amount A of
   * any currency, so that you can end up with some amount greater than A of that currency.
   * There are no transaction costs and you can trade fractional quantities.
   *
   * ANSWER: Suppose, 1 U.S. dollar bought 0.82 Euro, 1 Euro bought 129.7 Japanese Yen, 1 Japanese Yen bought 12
   * Turkish Lira, and 1 Turkish Lira bought 0.0008 U.S. dollars.
   * Then, by converting currencies, a trader can start with 1 U.S. dollar and buy 0.82 x 129.7 x 12 x 0.0008
   * ≅ 1.02 U.S dollars, thus doing arbitrage.
   *
   * To solve this problem, we will convert it to a graph problem, where every currency is a vertex, and the exchange
   * rates are directed edges. Note that it is going to be a complete graph (each pair of vertices is connected by an
   * edge).
   * The problem then becomes finding a cycle in a directed graph where the product of the edge weights is greater than
   * one.
   *
   * We know that log(ab) = log(a) + log(b) > 0. Taking the negative of both sides, -log(a) - log(b) < 0. Thus, if
   * we represent the edge weights are the negative logarithms of the exchange rates, the problem becomes finding a
   * negative cost cycle in a directed graph. Enter the Bellman-Ford algorithm!
   *
   * Since we are only interested in whether or not a negative cycle exists, not the actual shortest path (if any),
   * we don't need to store all values of i, just two (the current and the previous rows).
   *
   * Since this is a dense graph, time complexity is O(n^3). Space complexity is O(n).
   */
  def isArbitragePossible(rates: IndexedSeq[IndexedSeq[Double]]): Boolean = {
    val n = rates.size
    // dp[i][j] is the distance between the source and vertex j using no more than i edges
    // when i = 0, we can only go from the source to the source using the empty edge (length zero)
    val dp = Array.tabulate[Double](2, n)((i, v) => if (i == 0 && v == 0) 0d else Double.PositiveInfinity)

    val negLogRates = rates
      .map(_.map { r =>
        assert(r.notApproxEquals(0.0d), "Exchange rate must be a positive decimal")
        -math.log(r)
      })

    @tailrec
    def hasNegCycle(i: Int, v: Int): Boolean = {
      if (i <= n) {
        val cur = i % dp.length
        val prev = cur ^ 1
        dp(cur)(v) = math.min(
          dp(prev)(v),
          (0 until n)
            .filterNot(_ == v)
            .map(w => dp(prev)(w) + negLogRates(w)(v))
            .min
        )
        if (i == n && dp(cur)(v).notApproxEquals(dp(prev)(v))) true
        else if (v < n - 1) hasNegCycle(i, v + 1)
        else hasNegCycle(i + 1, 0)
      } else false
    }

    hasNegCycle(1, 0)
  }

  implicit val precision: Precision = Precision(0.01d)

  implicit class DoubleOps(val d: Double) extends AnyVal {
    def notApproxEquals(other: Double)(implicit p: Precision): Boolean = (d - other).abs >= p.p
  }

  /*
   * Suppose you are given a table of currency exchange rates, represented as a 2D array. Determine whether there is a
   * possible arbitrage: that is, whether there is some sequence of trades you can make, starting with some amount A of
   * any currency, so that you can end up with some amount greater than A of that currency.
   * There are no transaction costs and you can trade fractional quantities.
   *
   * ANSWER: Suppose, 1 U.S. dollar bought 0.82 Euro, 1 Euro bought 129.7 Japanese Yen, 1 Japanese Yen bought 12
   * Turkish Lira, and 1 Turkish Lira bought 0.0008 U.S. dollars.
   * Then, by converting currencies, a trader can start with 1 U.S. dollar and buy 0.82 x 129.7 x 12 x 0.0008
   * ≅ 1.02 U.S dollars, thus doing arbitrage.
   *
   * To solve this problem, we will convert it to a graph problem, where every currency is a vertex, and the exchange
   * rates are directed edges. Note that it is going to be a complete graph (each pair of vertices is connected by an
   * edge).
   * The problem then becomes finding a cycle in a directed graph where the product of the edge weights is greater than
   * one.
   *
   * We know that log(ab) = log(a) + log(b) > 0. Taking the negative of both sides, -log(a) - log(b) < 0. Thus, if
   * we represent the edge weights are the negative logarithms of the exchange rates, the problem becomes finding a
   * negative cost cycle in a directed graph. Enter the Bellman-Ford algorithm!
   *
   * Since we are only interested in whether or not a negative cycle exists, not the actual shortest path (if any),
   * we don't need to store all values of i, just two (the current and the previous rows).
   *
   * Since this is a dense graph, time complexity is O(n^3). Space complexity is O(n).
   */
  case class Precision(p: Double)

}
