package org.asarkar.epi

import scala.annotation.tailrec

package object arrays {
  /*
   * 5.1 The Dutch National Flag Problem
   */
  def dutchNationalFlag[T](a: Array[T])(implicit ordering: Ordering[T]): Unit = {
    def assertInvariants(lo: Int, hi: Int, lt: Int, gt: Int, pivot: Int): Unit = {
      assert((lt + 1 to gt).forall(x => ordering.lteq(a(x - 1), a(x))), "Invariant a[lt..gt] is sorted doesn't hold")
      assert((lo until lt).forall(x => ordering.lt(a(x), a(pivot))), "Invariant a[lo..lt-1] < pivot doesn't hold")
      assert((gt + 1 to hi).forall(x => ordering.gt(a(x), a(pivot))), "Invariant a[gt+1..hi] > pivot doesn't hold")
    }

    def sort(lo: Int, hi: Int): Unit = {
      partition3Way(a, lo, hi)
        .foreach { case (lt, gt, pivot) =>
          assertInvariants(lo, hi, lt, gt, pivot)
          sort(lo, lt - 1)
          sort(gt + 1, hi)
        }
    }

    sort(0, a.length - 1)
  }

  def partition3Way[T](a: Array[T], lo: Int, hi: Int)(implicit ordering: Ordering[T]): Option[(Int, Int, Int)] = {
    Iterator.iterate((lo, hi, lo + 1)) { case (lt, gt, pivot) =>
      if (ordering.lt(a(pivot), a(lt))) {
        swap(lt, pivot, a)
        (lt + 1, gt, pivot + 1)
      } else if (ordering.gt(a(pivot), a(gt))) {
        swap(gt, pivot, a)
        (lt, gt - 1, pivot)
      } else {
        (lt, gt, pivot + 1)
      }
    }
      .dropWhile(x => x._2 > x._3)
      .takeWhile(x => x._2 == x._3)
      .reduceOption(snd[(Int, Int, Int)])
  }

  private def swap[T](i: Int, j: Int, a: Array[T]): Unit = {
    val tmp = a(i)
    a(i) = a(j)
    a(j) = tmp
  }

  /*
   * 5.4 Advance through an array
   */
  def advance(a: Array[Int]): Boolean = {
    a.zipWithIndex
      .map { case (elem, idx) => (elem + idx, idx) }
      .foldLeft(0) { case (max, (elem, idx)) =>
        /*
         * current element is reachable from a previous one
         */
        if (idx <= max) {
          math.max(elem, max)
        } else {
          -1
        }
      } >= a.length - 1
  }

  /*
   * 5.5 Delete duplicates from a sorted array
   *
   * Returns the last index (exclusive) of the array until which there are no duplicates.
   */
  def dedup(a: Array[Int]): Int = {
    a.foldLeft(1) { (writeIndex, x) =>
      if (a(writeIndex - 1) != x) {
        a(writeIndex) = x
        writeIndex + 1
      } else writeIndex
    }
  }

  /*
   * 5.6 Buy and sell a stock once
   */
  def stock1(a: Array[Double]): Double = {
    a.foldLeft((Double.MaxValue, Double.MinValue)) { case ((minPrice, maxProfit), price) =>
      val min = math.min(minPrice, price)
      (min, math.max(maxProfit, price - min))
    }
      ._2
  }

  /*
   * 5.9 Enumerate all primes to n
   */
  def primes(n: Int): Set[Int] = {
    val primes = collection.mutable.BitSet()
    for (i <- 2 to n) {
      primes.add(i)
    }

    primes
      .iterator
      .foreach { p =>
        for (j <- p + p to n by p) {
          primes.remove(j)
        }
      }
    primes.toSet
  }

  /*
   * 5.10 Permute the elements of an array
   */
  def applyPermutation[T](a: Array[T], p: Array[Int]): Unit = {
    @tailrec
    def applyPermutation(i: Int): Unit = {
      if (i > a.length - 1) return

      Iterator.iterate(i) { j =>
        if (p(j) >= 0 && p(j) != j) {
          swap(p(j), i, a)
          val tmp = p(j)
          p(j) -= p.length
          tmp
        } else {
          i
        }
      }
        .drop(1) // drop first element so takeWhile doesn't become true to start with
        .takeWhile(_ != i)
        .foreach(_ => {}) // Iterator is lazy

      applyPermutation(i + 1)
    }

    applyPermutation(0)
  }

  /*
   * 5.19 Rotate a 2D array
   * https://stackoverflow.com/a/35438327/839733
   */
  def rotate2D(a: Array[Array[Int]]): Unit = {
    val n = a.length - 1
    val numLayers = a.length / 2

    for (row <- 0 until numLayers) {
      for (col <- row until (n - row)) {
        val temp1 = a(n - col)(row)
        val temp2 = a(n - row)(n - col)
        val temp3 = a(col)(n - row)
        val temp4 = a(row)(col)

        a(row)(col) = temp1
        a(n - col)(row) = temp2
        a(n - row)(n - col) = temp3
        a(col)(n - row) = temp4
      }
    }
  }
}
