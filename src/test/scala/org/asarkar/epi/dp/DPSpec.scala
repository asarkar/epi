package org.asarkar.epi.dp

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks

class DPSpec extends FlatSpec with TableDrivenPropertyChecks {
  "dp" should "compute the max sum over all subarrays" in {
    val data = Table(
      ("xs", "sum"),
      (IndexedSeq(904, 40, 523, 12, -335, -385, -124, 481, -31), 1479),
      (IndexedSeq(1, -3, 2, 1, -1), 3),
      (IndexedSeq(34, -50, 42, 14, -5, 86), 137),
      (IndexedSeq(-5, -1, -8, -9), 0),
      (IndexedSeq(-2, 1, -3, 4, -1, 2, 1, -5, 4), 6),
      (IndexedSeq(1, 2), 3),
      (IndexedSeq(1), 1),
      (IndexedSeq.empty[Int], 0)
    )

    forAll(data) { (xs, sum) =>
      maxSumSubarray(xs) shouldBe sum
    }
  }

  it should "compute the minimum number of edits needed to transform one string into another" in {
    val data = Table(
      ("t", "p", "d"),
      ("Saturday", "Sundays", 4),
      ("Hello", "Jello", 1),
      ("good", "good", 0),
      ("good", "goodbye", 3),
      ("Zeil", "trials", 4),
      ("kitten", "sitting", 3),
      ("intention", "execution", 5),
      ("mary had a little lamb", "twinkle twinkle little", 18)
    )

    forAll(data) { (t, p, d) =>
      levenshtein(t, p) shouldBe d
    }
  }

  it should "compute the length of the lowest common subsequence of two strings" in {
    val data = Table(
      ("t", "p", "lcs"),
      ("abcdgh", "aedfhr", 3),
      ("aggtab", "gxtxayb", 4),
      ("xmjyauz", "mzjawxu", 4),
      ("bdcaba", "abcbdab", 4),
      ("thisisatest", "testing123testing", 7)
    )

    forAll(data) { (t, p, lcs) =>
      org.asarkar.epi.dp.lcs(t, p) shouldBe lcs
    }
  }

  it should "search for a sequence in a 2D array" in {
    val grid = IndexedSeq(
      IndexedSeq(1, 2, 3),
      IndexedSeq(3, 4, 5),
      IndexedSeq(5, 6, 7)
    )

    twoDSearch(
      grid,
      IndexedSeq(1, 3, 4, 6)
    ) shouldBe true
    twoDSearch(
      grid,
      IndexedSeq(1, 2, 3, 4)
    ) shouldBe false
  }

  it should "find the min weight path in a triangle" in {
    minWeightPathInTriangle(
      IndexedSeq(
        IndexedSeq(2),
        IndexedSeq(4, 4),
        IndexedSeq(8, 5, 6),
        IndexedSeq(4, 2, 6, 2),
        IndexedSeq(1, 5, 2, 3, 4)
      )) shouldBe 15
  }

  it should "count the number of ways to climb stairs" in {
    climbStairs(3, 2) shouldBe 3
    climbStairs(4, 2) shouldBe 5
  }

  it should "compute the min messiness" in {
    val data = Table(
      ("words", "len", "min"),
      (IndexedSeq("a", "b", "c", "d"), 5, 8),
      (IndexedSeq("aaa", "bbb", "c", "d", "ee"), 11, 41),
      (IndexedSeq("aaa", "bbb", "c", "d", "ee", "ff"), 11, 20),
      (IndexedSeq("aaa", "bbb", "c", "d", "ee", "ff", "ggggggg"), 11, 36)
    )

    forAll(data) { (words, len, min) =>
      minMessiness(words, len) shouldBe min
    }
  }

  it should "compute the longest increasing subsequence" in {
    longestNondecreasingSubsequence(IndexedSeq(2, 4, 3, 5, 1, 7, 6, 9, 8)) shouldBe 5
  }
}
