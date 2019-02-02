package org.asarkar.epi

import java.awt.Rectangle

import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.util.Random

/*
 * http://www.catonmat.net/blog/low-level-bit-hacks-you-absolutely-must-know/
 */
package object primitives {
  private val ParityTable = Map(0 -> 0, 1 -> 1, 2 -> 1, 3 -> 0)

  /*
   * 4.1 Computing the parity of a 64-bit word
   *
   * Parity is associative, so, parity of 11111110 = parity of 1111 ^ parity of 1110
   * We use a look up table for sufficiently small values; larger the table, fewer the iterations.
   *
   * Complexity: O(log2n). If the look up is larger, we could avoid recursing for small x.
   */
  def parity(x: Long, shift: Int = 32): Integer = {
    /*
     * 1L is necessary because for an Integer type, only the five lowest-order bits of the right-hand operand are
     * used as the shift distance. Basically, the shift distance is calculated as RHS % 32,
     * so RHS = 32 shifts by zero (no shift). Subtracting 1 creates a mask with all lowest shift bits set.
     */
    def lo(n: Int) = x & ((1L << n).toInt - 1)

    if (x <= 3) return ParityTable(x.toInt)

    /*
     * >>> unsigned right shift - logical right shift, fills vacant positions with 0
     * >> right shift - arithmetic right shift, fills vacant positions with sign bit
     */
    val h = x >>> shift
    val l = lo(shift)

    /*
     * parity of 11111110 = parity of 1111 ^ parity of 1110
     */
    parity(h, shift >> 1) ^ parity(l, shift >> 1)
  }

  /*
   * 4.2 Swap bits
   */
  def swap(x: Long, i: Byte, j: Byte) = {
    def isValidIndex(i: Byte) = i < java.lang.Long.BYTES && i >= 0

    if (!isValidIndex(i) || !isValidIndex(j)) {
      throw new IndexOutOfBoundsException(s"One of $i and $j is outside the range (0, 63)")
    }
    val ithSet = (x & (1 << i)) > 0
    val jthSet = (x & (1 << j)) > 0

    // if the bits differ, flip the values
    if (ithSet != jthSet) {
      (x ^ (1 << i)) ^ (1 << j)
    } else {
      x
    }
  }

  /*
   * 4.8 Reverse digits
   */
  def reverse(x: Long): Long = {
    @tailrec
    def reverse(x: Long, result: Long): Long = {
      if (x == 0) result
      else reverse(x / 10, result * 10 + x % 10)
    }

    /*
     * -1 % 10 = -1, hence we need to use the abs value
     */
    val y = reverse(math.abs(x), 0)
    if (x < 0) -y else y
  }

  /*
   * 4.9 Check if a decimal integer is a palindrome
   */
  def isPalindrome(x: Long): Boolean = {
    @tailrec
    def isPalindrome(x: Long, len: Int): Boolean = {
      if (len <= 1) return true
      /*
       * ex: 151751 / pow(10, 5) = 151751 / 100000 = 1.52
       */
      val pow = math.pow(10, len - 1).toInt
      val msd = x / pow
      val lsd = x % 10

      msd == lsd && isPalindrome(x / 10 - (msd * pow / 10), len - 2)
    }

    /*
    * ex: log10(151751) = 5.18
    */
    val len = if (x <= 0) 1 else math.log10(x).toInt + 1

    x >= 0 && isPalindrome(x, len)
  }

  /*
   * 4.10 Generate uniform random numbers
   */
  def random(lo: Int, hi: Int): Int = {
    require(lo >= 0 && hi > lo, "Range must be non-negative")
    /*
     * While mathematically this is correct, there is a risk of miscalculation due to imprecise floating-point
     * arithmetic. See https://stackoverflow.com/a/3305400/839733
     */
    val numAttempts = math.log10(hi - lo + 1).toInt + 1
    val rand = new Random()

    val random = (0 until numAttempts)
      .map(i => (i, if (rand.nextBoolean()) 1 else 0))
      .foldLeft(0) { case (acc, (position, digit)) => acc + math.pow(2, position).toInt * digit }

    Iterator.iterate(random)(_ => random)
      .map(_ + lo)
      .dropWhile(_ > hi)
      .next()
  }

  def intersection(r1: Rectangle, r2: Rectangle): Option[Rectangle] = {
    class RectangleOps(val a: Rectangle) {
      def leftOf(that: Rectangle): Boolean = a.x + a.width < that.x

      def topOf(that: Rectangle): Boolean = a.y + a.height < that.y
    }

    implicit def toRichRectangle(r: Rectangle): RectangleOps = new RectangleOps(r)

    def minMax(a: Seq[Int]): (Int, Int) = {
      a.foldLeft((a.head, a.head)) { case ((min, max), e) => (math.min(min, e), math.max(max, e)) }
    }

    if (r1.leftOf(r2) || r2.leftOf(r1) || r1.topOf(r2) || r2.topOf(r1)) {
      None
    } else {
      val xs = (r1.x to r1.x + r1.width).intersect(r2.x to r2.x + r2.width)
      val ys = (r1.y to r1.y + r1.height).intersect(r2.y to r2.y + r2.height)

      val (x1, x2) = minMax(xs)
      val (y1, y2) = minMax(ys)

      Some(new Rectangle(x1, y1, x2 - x1, y2 - y1))
    }
  }
}
