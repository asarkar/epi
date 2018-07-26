package org.abhijitsarkar.arrays

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class ArraysSpec extends FlatSpec {
  "arrays" should "sort an array using 3-way quicksort" in {
    val a = Array(7, 1, 3, 5, 3, 9)
    dutchNationalFlag(a)
    a shouldBe sorted
  }

  it should "advance through an array" in {
    advance(Array(3, 3, 1, 0, 2, 0, 1)) shouldBe true
    advance(Array(3, 2, 0, 0, 2, 0, 1)) shouldBe false
  }

  it should "dedup array" in {
    dedup(Array(1, 1, 1, 2, 2, 2, 3, 4)) shouldBe 4
    dedup(Array(1, 2, 3, 3, 4, 4)) shouldBe 4
    dedup(Array(1, 2, 3, 4)) shouldBe 4
  }

  it should "determine the max profit from buying and selling a stick once" in {
    stock1(Array(310, 315, 275, 295, 260, 270, 290, 230, 255, 250)) shouldBe 30
  }

  it should "generate all primes to n" in {
    primes(10) should contain only(2, 3, 5, 7)
  }

  it should "permute the elements of an array" in {
    val a = Array('a', 'b', 'c', 'd', 'e')
    applyPermutation(a, Array(4, 3, 2, 0, 1))

    a shouldBe Array('d', 'e', 'c', 'b', 'a')
  }

  it should "rotate a 2D array" in {
    val a = Array(
      Array(1, 2, 3, 4),
      Array(5, 6, 7, 8),
      Array(9, 10, 11, 12),
      Array(13, 14, 15, 16)
    )
    rotate2D(a)
    a shouldBe Array(
      Array(13, 9, 5, 1),
      Array(14, 10, 6, 2),
      Array(15, 11, 7, 3),
      Array(16, 12, 8, 4)
    )
  }
}
