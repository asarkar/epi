package org.abhijitsarkar.primitives

import java.awt.Rectangle

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.OptionValues._
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor2}

class PrimitivesSpec extends FlatSpec with TableDrivenPropertyChecks {
  "primitives" should "compute the parity of a word" in {
    parity(254L) shouldBe 1
    parity(1742346774L) shouldBe 0
  }

  it should "swap 2 bits of a decimal number" in {
    /*
     * 43 = 101011, 42 = 101010
     */
    swap(43L, 5, 0) shouldBe 43
    swap(42L, 5, 0) shouldBe 11
    an[IndexOutOfBoundsException] should be thrownBy swap(42L, -1, 0)
  }

  it should "reverse digits of a decimal number" in {
    reverse(321) shouldBe 123
    reverse(-321) shouldBe -123
  }

  it should "check if a decimal integer is a palindrome" in {
    val numbers: TableFor2[Long, Boolean] =
      Table(
        ("n", "palindrome"),
        (0, true),
        (1, true),
        (7, true),
        (11, true),
        (121, true),
        (33, true),
        (2147447412, true),
        (-1, false),
        (12, false),
        (100, false),
        (2147483647, false)
      )

    forAll(numbers) { (n: Long, palindrome: Boolean) =>
      isPalindrome(n) shouldBe palindrome
    }
  }

  it should "generate a random number between 1 and 6" in {
    (1 to 10)
      .map(_ => random(1, 6))
      .forall(i => i >= 1 && i <= 6) shouldBe true
  }

  it should "check if 2 rectangles intersect" in {
    val r1 = new Rectangle(1, 2, 3, 4)
    val r2 = new Rectangle(5, 3, 2, 4)

    intersection(r1, r2) shouldBe empty

    val r3 = new Rectangle(0, 1, 3, 4)
    val r4 = new Rectangle(1, 2, 2, 4)

    /*
     * see src/test/resources/rect-intersection.jpg
     */
    val r = intersection(r3, r4)
    r.value.x shouldBe 1
    r.value.y shouldBe 2
    r.value.width shouldBe 2
    r.value.height shouldBe 3
  }
}
