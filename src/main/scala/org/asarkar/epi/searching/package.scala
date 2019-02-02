package org.asarkar.epi

import scala.annotation.tailrec

import arrays.partition3Way

package object searching {
  /*
   * 11.1 Search a sorted array for first occurrence of k
   *
   * Regular Binary search, but don't stop on first match, keep looking on the left.
   * Obviously, if k is found at index i, the first occurrence is either index i, or somewhere on its left.
   */
  def searchFirstOfK[T](xs: IndexedSeq[T], k: T)(implicit ordering: Ordering[T]): Int = {
    Iterator.iterate((0, xs.size - 1, -1)) { case (lo, hi, first) =>
      val mid = lo + (hi - lo) / 2

      if (ordering.lt(xs(mid), k)) {
        (mid + 1, hi, first)
      } else if (ordering.gt(xs(mid), k)) {
        (lo, mid - 1, first)
      } else {
        (lo, mid - 1, mid)
      }
    }
      .dropWhile { case (lo, hi, _) => lo < hi }
      // when (i, i, first), we still need to look at xs(i). if we took one element, we'd miss xs(i)
      .take(2)
      .map(_._3)
      .reduce(snd[Int])
  }

  /*
   * 11.2 Search a sorted array for entry equal to its index (a.k.a. fixed point)
   */
  def fixedPoint[T](xs: IndexedSeq[T])(implicit ordering: Ordering[T], n: Numeric[T]): Option[Int] = {
    Iterator.iterate((0, xs.size - 1, false)) { case (lo, hi, _) =>
      val mid = lo + (hi - lo) / 2
      val v = n.toInt(xs(mid))

      if (v == mid) {
        (mid, mid, true)
      }
      // Since the element are distinct, if v > mid, v + 1 > mid + 1
      else if (v > mid) {
        (lo, mid - 1, false)
      } else { // same logic as above
        (mid + 1, hi, false)
      }
    }
      .dropWhile { case (lo, hi, found) => lo < hi && !found }
      .take(1)
      .map(_._1)
      .reduceOption(snd[Int])
  }

  /*
   * 11.3 Search a cyclically sorted array
   */
  def searchCircularArray[T](xs: IndexedSeq[T])(implicit ordering: Ordering[T]): Int = {
    Iterator.iterate((0, xs.size - 1, false)) { case (lo, hi, _) =>
      val mid = lo + (hi - lo) / 2

      // mid to n is increasing sequence, min must be on the left of mid
      if (ordering.lt(xs(mid), xs.last)) {
        (lo, mid, false)
      } // lo to mid is decreasing sequence, min must be on the right of mid
      else if (ordering.gt(xs(mid), xs.last)) {
        (mid + 1, hi, false)
      } else {
        (mid, mid, true)
      }
    }
      .dropWhile { case (lo, hi, found) => lo < hi && !found }
      .take(1)
      .map(_._1)
      .reduce(snd[Int])
  }

  /*
   * 11.6 Search in a 2D sorted array
   */
  def search2DArray[T](xs: IndexedSeq[IndexedSeq[T]], k: T)(implicit ordering: Ordering[T]): (Int, Int) = {
    val m = xs.size
    val n = xs.head.size

    def isValid(row: Int, col: Int): Boolean = {
      row >= 0 && row < m && col >= 0 && col < n
    }

    val it = Iterator.iterate((0, n - 1, false)) { case (row, col, _) =>
      val x = xs(row)(col)
      // cannot be in this row since all values on the right of this element have been examined; move to next row
      if (ordering.gt(k, x)) {
        (row + 1, col, false)
      }
      // cannot be in this column since all values on the top of this element have been examined; move to previous col
      else if (ordering.lt(k, x)) {
        (row, col - 1, false)
      } else {
        (row, col, true)
      }
    }
      .dropWhile { case (row, col, found) => isValid(row, col) && !found }
      .take(1)
      .map(x => (x._1, x._2))
      // if not found, last element is not valid
      .filter(x => isValid(x._1, x._2))

    if (it.hasNext) {
      it.next()
    } else {
      (-1, -1)
    }
  }

  /*
   * 11.8 Find the kth largest element
   */
  def kthLargest[T](a: Array[T], k: Int)(implicit ordering: Ordering[T]): T = {
    // 1st largest element is the largest element in the array; nth largest element is the smallest.
    // partition3Way partitions in non-decreasing order, this problem requires non-increasing order.
    val i = a.length - k + 1

    @tailrec
    def partition(lo: Int, hi: Int): T = {
      partition3Way(a, lo, hi)
        .map(_._3) match {
        case Some(pivot) =>
          // pivot is the ith element
          if (pivot == i - 1) {
            a(pivot)
          }
          // there are at least i elements less than or equal to pivot, keep looking on the left. discard element
          // at pivot since we already checked it and was not the ith element
          else if (pivot >= i) {
            partition(lo, pivot - 1)
          }
          // there are at least i elements greater than or equal to pivot, keep looking on the right. discard element
          // at pivot since we already checked it and was not the ith element
          else {
            partition(pivot + 1, hi)
          }
        // lo = hi
        case _ => a(hi)
      }
    }

    partition(0, a.length - 1)
  }
}
