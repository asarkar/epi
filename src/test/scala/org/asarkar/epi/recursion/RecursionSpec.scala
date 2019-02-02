package org.asarkar.epi.recursion

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

import scala.collection.mutable

class RecursionSpec extends FlatSpec {
  "recursion" should "solve the towers of Hanoi problem" in {
    hanoi(3) // assertions done in code
  }

  it should "generate all nonattacking placements of n-Queens" in {
    val solutions = nQueen(4)
    solutions.size shouldBe 2
    solutions should contain(IndexedSeq(1, 3, 0, 2))
    solutions should contain(IndexedSeq(2, 0, 3, 1))
  }

  it should "generate permutations of an array" in {
    val perm = permutations(mutable.IndexedSeq(7, 3, 5))
    perm.size shouldBe 6
    perm should contain(Seq(7, 3, 5))
    perm should contain(Seq(7, 5, 3))
    perm should contain(Seq(3, 7, 5))
    perm should contain(Seq(3, 5, 7))
    perm should contain(Seq(5, 3, 7))
    perm should contain(Seq(5, 7, 3))
  }

  private def fact(n: Int): Int = {
    if (n == 0) 1
    else n * fact(n - 1)
  }

  it should "generate the power set" in {
    val a = powerSet1(3)
    a should have size (1 << 3)
    a should contain theSameElementsAs Seq(
      Seq(),
      Seq(1), Seq(2), Seq(3),
      Seq(2, 1), Seq(3, 1), Seq(3, 2),
      Seq(3, 2, 1)
    )

    val b = powerSet2(3)
    b should have size (1 << 3)
    b should contain theSameElementsAs Seq(
      Seq(),
      Seq(1), Seq(2), Seq(3),
      Seq(1, 2), Seq(1, 3), Seq(2, 3),
      Seq(1, 2, 3)
    )
  }

  it should "generate all subsets of size k" in {
    val a = subsets(3, 2)
    a should have size fact(3) / (fact(2) * fact(3 - 2))
    a should contain theSameElementsAs Seq(
      Seq(2, 1), Seq(3, 1), Seq(3, 2)
    )

    val xs = subsets(5, 2)
    xs should have size fact(5) / (fact(2) * fact(5 - 2))
    xs should contain theSameElementsAs Seq(
      Seq(2, 1), Seq(3, 1), Seq(4, 1), Seq(5, 1),
      Seq(3, 2), Seq(4, 2), Seq(5, 2),
      Seq(4, 3), Seq(5, 3),
      Seq(5, 4)
    )

    val ys = subsets(5, 3)
    ys should have size fact(5) / (fact(3) * fact(5 - 3))
    ys should contain theSameElementsAs Seq(
      Seq(3, 2, 1), Seq(4, 2, 1), Seq(5, 2, 1),
      Seq(4, 3, 1), Seq(5, 3, 1),
      Seq(5, 4, 1),
      Seq(4, 3, 2), Seq(5, 3, 2),
      Seq(5, 4, 2),
      Seq(5, 4, 3)
    )

    val zs = subsets(5, 4)
    zs should have size fact(5) / (fact(4) * fact(5 - 4))
    zs should contain theSameElementsAs Seq(
      Seq(4, 3, 2, 1), Seq(5, 3, 2, 1),
      Seq(5, 4, 2, 1),
      Seq(5, 4, 3, 1),
      Seq(5, 4, 3, 2)
    )
  }

  it should "generate gray codes" in {
    grayCode(3) should contain theSameElementsAs Seq(
      Seq(0, 0, 0), Seq(0, 0, 1),
      Seq(0, 1, 1), Seq(0, 1, 0),
      Seq(1, 1, 1), Seq(1, 1, 0),
      Seq(1, 0, 0), Seq(1, 0, 1)
    )
  }
}
