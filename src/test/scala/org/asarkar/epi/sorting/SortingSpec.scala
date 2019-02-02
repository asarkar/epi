package org.asarkar.epi.sorting

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.OptionValues._

class SortingSpec extends FlatSpec {
  "sorting" should "compute the intersection of two sorted arrays" in {
    val xs = IndexedSeq(2, 3, 3, 5, 7, 11)
    val ys = IndexedSeq(3, 3, 7, 15, 31)

    intersection(xs, ys).toSeq should contain only(3, 7)
  }

  it should "compute the smallest nonconstructible value" in {
    val xs = Seq(12, 2, 1, 15, 2, 4)
    smallestNonconstructibleValue(xs).value shouldBe 10
  }

  it should "merge intervals" in {
    val xs = Seq((-4, -1), (0, 2), (3, 6), (7, 9), (11, 12), (14, 17))
    mergeIntervals(xs, (1, 8)) should contain inOrderOnly((-4, -1), (0, 9), (11, 12), (14, 17))
  }

  it should "partion an array with repeated entries" in {
    val a = Array(
      ("Greg", 14),
      ("John", 12),
      ("Andy", 11),
      ("Jim", 13),
      ("Phil", 12),
      ("Bob", 13),
      ("Chip", 13),
      ("Tim", 14)
    )
    groupByAge(a)
    println(a.deep)
  }
}
