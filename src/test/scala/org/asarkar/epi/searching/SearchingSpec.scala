package org.asarkar.epi.searching

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor2}

class SearchingSpec extends FlatSpec with TableDrivenPropertyChecks {
  "search" should "find first occurrence of an element" in {
    val xs = IndexedSeq(-14, -10, 2, 108, 108, 243, 285, 285, 285, 401)
    searchFirstOfK(xs, 108) shouldBe 3
    searchFirstOfK(xs, 999) shouldBe -1
  }

  it should "find fixed point in a sorted array" in {
    val xs = IndexedSeq(-2, 0, 2, 3, 3, 6, 7, 9)
    fixedPoint(xs) should contain oneOf(2, 3)
  }

  it should "find the min in a cyclically sorted array" in {
    val xs = IndexedSeq(378, 478, 550, 631, 103, 203, 220, 234, 279, 368)
    searchCircularArray(xs) shouldBe 4
  }

  it should "search 2D array" in {
    val xs = IndexedSeq(
      IndexedSeq(-1, 2, 4, 4, 6),
      IndexedSeq(1, 5, 5, 9, 21),
      IndexedSeq(3, 6, 6, 9, 22),
      IndexedSeq(3, 6, 8, 10, 24),
      IndexedSeq(6, 8, 9, 12, 25),
      IndexedSeq(8, 10, 12, 13, 40)
    )

    search2DArray(xs, 7) shouldBe(-1, -1)
    search2DArray(xs, 8) shouldBe(3, 2)
  }

  it should "find the kth largest element" in {
    val data: TableFor2[Array[Int], Map[Int, Int]] =
      Table(
        ("a", "k -> v"),
        (Array(1, 3, 5, 3, 7, 9), Map(1 -> 9, 6 -> 1)), // sorted (1, 3, 3, 5, 7, 9)
        (Array(3, 2, 1, 5, 4), Map(1 -> 5, 3 -> 3, 5 -> 1)) // sorted (1, 2, 3, 4, 5)
      )

    forAll(data) { (a: Array[Int], map: Map[Int, Int]) =>
      map.foreach { case (k, v) => kthLargest(a, k) shouldBe v }
    }
  }
}
