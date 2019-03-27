package org.asarkar.epi

package object dp {
  /*
   * Find the maximum sum over all subarrays of a given array.
   *
   * ANSWER: We use Kadane's algorithm that takes advantage of the optimal substructure of the problem.
   *
   * Given array A, let M[i] be the maximum sum of the subarray ending at index i. M[i+1] could be
   * obtained by extending the M[i] with A[i+1]. Another possibility is that M[i] is a very small
   * value (perhaps many of the elements are negative integers), and we are better off just taking
   * A[i+1] by itself.
   * Thus, the recurrence relation is:
   * M[i] = max{ M[i - 1] + A[i], A[i] }, ∀ i = 1 to n - 1
   *      = 0 if i = 0.
   *
   * However, since each value of M[i] is only computed once, because this algorithm doesn't exhibit
   * overlapping subproblems (recomputing the same values over and over). Thus, it may not be called
   * DP.
   *
   * If the array contains only negative numbers, the maximum sum would be zero, since we would not take any elements.
   */
  def maxSumSubarray(xs: IndexedSeq[Int]): Int = {
    // stores the maximum sum of the subarray ending at index i
    val dp = Array.ofDim[Int](xs.size)
    dp(0) = xs.head

    xs
      .tail
      .zipWithIndex
      .foldLeft(0) { case (max, (x, i)) =>
        dp(i + 1) = math.max(dp(i) + x, x)
        math.max(max, dp(i + 1))
      }
  }

  /*
   * 16.2 Write a program that takes two strings and computes the minimum number of edits needed to transform the
   * first string into the second string.
   *
   * ANSWER:
   * Let dp(i)(j) be the minimum number of edits needed to transform the first i characters of the text string t
   * into the first j characters of the pattern string p.
   *
   * if t(i) == p(j)
   *   no editing is required.
   * else
   *   we may either substitute t(i) with p(j), or
   *   we may insert p(j) after t(i), or
   *   we may delete t(i)
   *
   * Whatever option we choose, we add it to the number of edits required to transform the rest of t to p.
   * Which option do we choose? That's easy, we choose the one with the minimum number of edits; after all,
   * that's what we are after.
   *
   * The base case is when one of the strings is empty. To transform the non-empty string into the empty one,
   * we delete all characters of the non-empty string which takes as many edits (deletes) as the length of the
   * string.
   *
   * We can later backtrack starting from dp(n)(m) and reconstruct the sequence of edits by comparing each cell
   * with its immediate neighbors on the left (insertion), top(deletion), and diagonally up (same or substitution).
   *
   * OPTIMIZATIONS:
   * If we don't need to reconstruct the edit sequence, we can save space by storing only the last (and current) rows.
   */


  def levenshtein(t: String, p: String): Int = {
    val costs = EditCost.values.map(_ -> 1).toMap
    val init = (i: Int, j: Int) => if (i == 0) j else if (j == 0) i else -1
    editDistance(t, p, costs, init, dp => dp(t.length)(p.length))
  }

  /*
   * The Longest Common Subsequence (LCS) problem can be formulated as edit distance problem if we maximize the number
   * of identical matches by prohibiting substitution of nonidentical characters. This can be easily done by making
   * substitution more expensive than insertion and deletion. Once we have value of edit distance e, LCS is given by:
   * e = |t| + |p| - 2|LCS| => LCS = (|t| + |p| - e) / 2
   *
   * The above should be intuitive. Concat both strings, delete the characters that don't match, we're left with
   * twice the characters that match.
   *
   * LCS problem can also be solved independent of edit distance by recognizing the following recurrence:
   *
   * if t(i) == p(j)
   *   LCS(i, j) = 1 + LCS(i - 1, j - 1)
   * else
   *   if t(i) is included, then p(j) can be ignored, and LCS(i, j) = LCS(i, j - 1)
   *   else t(i) can be ignore, and LCS(i, j) = LCS(i - 1, j)
   *
   * As usual, since we don't know whether t(i) is included in the LCS or not, we consider both possibilities,
   * and take the maximum of them.
   */
  def lcs(t: String, p: String): Int = {
    val costs = EditCost.values
      .map(x => x -> (if (x == EditCost.Substitution) 2 else 1))
      .toMap
    val init = (i: Int, j: Int) => if (i == 0) j else if (j == 0) i else -1
    val x = editDistance(t, p, costs, init, dp => dp(t.length)(p.length))
    (t.length + p.length - x) >> 1
  }

  /*
   * If we want to allow matching against a substring of the text, rather than the whole, we can do so by adapting
   * 'init' and 'goal' functions accordingly.
   * dp(i)(0) = 0 for all i in [0, t.length) because there's no penalty for starting a match somewhere in the middle.
   * The result is no longer given by dp(t.length)(p.length) but the min { dp(i)(p.length) } for all i in [0, t.length).
   */
  // def substring(t: String, p: String): Int

  private object EditCost extends Enumeration {
    val Substitution, Deletion, Insertion = Value
  }

  private def editDistance(
                            t: String,
                            p: String,
                            costs: Map[EditCost.Value, Int],
                            init: (Int, Int) => Int,
                            goal: Array[Array[Int]] => Int
                          ): Int = {
    val n = t.length
    val m = p.length
    val dp = Array.tabulate[Int](n + 1, m + 1)(init)

    for (i <- 1 to n)
      for (j <- 1 to m) {
        dp(i)(j) = if (t(i - 1) == p(j - 1)) dp(i - 1)(j - 1)
        else Seq(
          costs(EditCost.Substitution) + dp(i - 1)(j - 1), // substitute s(i) by t(j), and transform the rest of s(0, i-1)
          costs(EditCost.Insertion) + dp(i)(j - 1), // insert t(j) at the end of s, and transform the rest s(0, i)
          costs(EditCost.Deletion) + dp(i - 1)(j) //delete s(i), and transform the rest of s(0, i-1)
        )
          .min
      }

    goal(dp)
  }

  /*
   * 16.5 Search for a sequence in a 2D array
   *
   * ANSWER: The idea is quite simple. Given array 'a', we initiate a search from a(i)(j) by matching it with the
   * first item of the pattern. If a match is found, we recursively expand the match by comparing neighbors of a(i)(j)
   * with the next element of the pattern. Otherwise, we do this for all i, j, since a successful match may begin
   * anywhere in 'a'.
   *
   * We remember the result of matching a(i)(j) with p(k) for all i, j, k, such that once we find a dead end,
   * we avoid it later. Note that it is not sufficient to remember the cell a(i)(j), but also the p(k) that was
   * matched since a different value of k might produce a different match result.
   */
  def twoDSearch(grid: IndexedSeq[IndexedSeq[Int]], pattern: IndexedSeq[Int]): Boolean = {
    def neighbors(row: Int, col: Int): Seq[(Int, Int)] = {
      Seq(
        (row - 1, col),
        (row + 1, col),
        (row, col - 1),
        (row, col + 1)
      )
        .filter(x => grid.isDefinedAt(x._1) && grid(x._1).isDefinedAt(x._2))
    }

    val visited = collection.mutable.Map.empty[(Int, Int, Int), Boolean]

    def visit(row: Int, col: Int, i: Int): Boolean = {
      val id = (row, col, i)

      if (visited.contains(id)) {
        println(s"Already visited $id")
        visited(id)
      } else {
        val matches = grid(row)(col) == pattern(i)
        visited((row, col, i)) = matches

        println(s"""grid($row, $col}) ${if (matches) "=" else "!="} pattern($i)""")

        matches && (!pattern.isDefinedAt(i + 1) || neighbors(row, col).exists(x => visit(x._1, x._2, i + 1)))
      }
    }

    println(s"grid = $grid, pattern = $pattern")
    grid.indices
      .exists(row => grid(row).indices.exists(col => visit(row, col, 0)))
  }

  /*
   * 16.8 Find the minimum weight path in a triangle
   */
  def minWeightPathInTriangle(t: IndexedSeq[IndexedSeq[Int]]): Int = {
    def neighbors(row: Int, col: Int): Seq[(Int, Int)] = {
      Seq(
        (row + 1, col),
        (row + 1, col + 1)
      )
        .filter(x => t.isDefinedAt(x._1) && t(x._1).isDefinedAt(x._2))
    }

    Iterator.iterate((0, 0), 0) { case ((row, col), min) =>
      neighbors(row, col) match {
        case Seq() => ((-1, -1), min + t(row)(col))
        case xs =>
          val y = xs.minBy(x => t(x._1)(x._2))
          (y, min + t(row)(col))
      }
    }
      .dropWhile(x => t.isDefinedAt(x._1._1))
      .take(1)
      .map(_._2)
      .next()
  }

  /*
   * 16.10 Find the number of moves to climb stairs
   *
   * ANSWER: Observe the following table:
   *
   * +---+------+----------------------------------------------+
   * | n |  k   |                    steps                     |
   * +---+------+----------------------------------------------+
   * | 1 |    2 |    {(1)}                                     |
   * | 2 |    2 |    {(1,1),(2)}                               |
   * | 3 |    2 |    {(1,1,1),(1,2),(2,1)}                     |
   * | 4 |    2 |    {(1,1,2),(2,2),(1,1,1,1),(1,2,1),(2,1,1)} |
   * +---+------+----------------------------------------------+
   *
   * Consider n = 2. To go to the second step, we could go to the first step, and take whatever number of steps left
   * from step 1 to 2, or we could directly go to the second step (from step 0, aka the ground). Similar
   * reasoning shows to get to the nth step, we go to one of the n - ith step, where i varies from 1 to k, and from
   * the n - ith step, take n - i steps to get to the nth step. As far as the number of steps go, we tack on n - i
   * steps to the end of each of the various number of ways we could have got to the n - ith step. As far as number
   * of ways to get to step n, it is the sum of all n - i possibilities, where i varies from 1 to k.
   */
  def climbStairs(n: Int, k: Int): Int = {
    val dp = Array.tabulate[Int](n + 1)(i => if (i <= 1) 1 else 0)

    for (i <- 2 to n; j <- 1 to k)
      dp(i) += dp(i - j)

    dp.last
  }

  /*
   * 16.11 The pretty printing problem
   *
   * ANSWER: Let the minimum messiness of a line be defined by the position of the word it ends with. In other words,
   * minimum messiness of a line ending with the i-th word is dp(i). Now if we somehow knew which word the line started
   * with, we could compute dp(i). Since we don't know that, we are going to compute the minimum messiness using all
   * the prefixes until the i-th word, for which we have i choices ({w₁}, {w₁, w₂}, {w₁, w₂, w₃}, ...,
   * {w₁, w₂, ..., wᵢ}) and take the minimum of the bunch. For each prefix, we also need to add the minimum messiness
   * of the previous subproblem dp(j - 1), where j is the position of the first word.
   */
  def minMessiness(words: IndexedSeq[String], len: Int): Int = {
    val n = words.size
    val dp = Array.ofDim[Int](n + 1)

    def messiness(i: Int, j: Int): Int = {
      assert(j < i, s"$j must be less than $i")
      val sublist = words.slice(j, i)
      val cost = sublist.map(_.length).sum + // total number of characters occupied by all the words
        sublist.size - 1 // total number of spaces, one each between two words

      if (len >= cost) math.pow(len - cost, 2).intValue
      else Int.MinValue
    }

    for (i <- 1 to n) {
      dp(i) = (i - 1 to 0 by -1)
        .map(j => dp(j) + messiness(i, j))
        .takeWhile(_ >= 0)
        .min
    }

    dp.last
  }

  /*
   * 16.12 Find the longest nondecreasing subsequence
   *
   * ANSWER: Let L(i) be the length of the longest nondecreasing subsequence ending with the i-th element.
   * Then L(i) is simply one more than the length of the longest nondecreasing subsequence ending with some element
   * j < i. Since we don't know j, we try them all, and take the maximum.
   *
   * There exists a O(nlog(n)) algorithm (this one is clearly O(n²)) but it's considerably more complicated.
   */
  def longestNondecreasingSubsequence(xs: IndexedSeq[Int]): Int = {
    val dp = Array.ofDim[Int](xs.size)

    for (i <- xs.indices)
      dp(i) = 1 + (0 until i)
        // ignore larger elements since those can't be part of the longest nondecreasing subsequence
        .filter(j => xs(j) <= xs(i))
        .foldLeft(0) { (max, j) => math.max(max, dp(j)) }

    dp.last
  }
}
