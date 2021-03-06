package org.asarkar.epi.lists

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.OptionValues._

class ListsSpec extends FlatSpec {
  "lists" should "merge two sorted lists" in {
    val xs1 = LinkedList(1, 3, 5)
    val ys1 = LinkedList(2, 4, 6)

    merge(xs1, ys1).toSeq shouldBe Seq.range(1, 7)

    val xs2 = LinkedList(1, 3, 5)
    val ys2 = LinkedList(1)

    /*
     * inOrder must not contain any duplicated values
     */
    merge(xs2, ys2).toSeq shouldBe Seq(1, 1, 3, 5)

    val xs3 = LinkedList(2)
    val ys3 = LinkedList(1)

    merge(xs3, ys3).toSeq should contain inOrderOnly(1, 2)

    val xs4 = LinkedList(1, 7, 9)
    val ys4 = LinkedList(3, 5)

    merge(xs4, ys4).toSeq shouldBe Seq.range(1, 10, 2)
  }

  it should "reverse a sublist" in {
    val xs1 = ListNode(11, 3, 5, 7, 2)

    reverseSublist(xs1, 2, 4).toSeq should contain inOrderOnly(11, 7, 5, 3, 2)

    val xs2 = ListNode(11, 3, 5, 7, 2)

    reverseSublist(xs2, 1, 10).toSeq should contain inOrderOnly(2, 7, 5, 3, 11)
  }

  it should "detect cycle" in {
    val xs1 = LinkedList(1, 2, 3, 4, 5, 6, 7, 4)
    startOfCycle(xs1) should contain(4)

    val xs2 = LinkedList(1, 2, 3)
    startOfCycle(xs2) shouldBe empty
  }

  it should "detect overlap" in {
    val xs = ListNode(3, 5, 7, 2)
    val ys = ListNode(11, xs)

    overlapNoCycle(xs, ys).value.datum shouldBe 3
  }

  it should "implement even-odd merge" in {
    val xs = ListNode(1, 2, 3, 4, 5)
    evenOdd(xs).toSeq should contain inOrderOnly(1, 3, 5, 2, 4)
  }
}
