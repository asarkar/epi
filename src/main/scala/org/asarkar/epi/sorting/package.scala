package org.asarkar.epi

import scala.annotation.tailrec
import scala.collection.mutable

package object sorting {
  def binarySearch[T](xs: IndexedSeq[T], x: T)(implicit ordering: Ordering[T]): Option[Int] = {
    binarySearch(xs, 0, xs.size - 1, x)
  }

  @tailrec
  def binarySearch[T](xs: IndexedSeq[T], lo: Int, hi: Int, x: T)(implicit ordering: Ordering[T]): Option[Int] = {
    val mid = lo + (hi - lo) / 2

    if (lo > hi) {
      None
    } else if (ordering.equiv(xs(mid), x)) {
      Some(mid)
    } else if (ordering.gt(xs(mid), x)) {
      binarySearch(xs, lo, mid - 1, x)
    } else {
      binarySearch(xs, mid + 1, hi, x)
    }
  }

  /*
   * 13.1 Compute the intersection of two sorted arrays
   */
  def intersection[T](xs: IndexedSeq[T], ys: IndexedSeq[T])(implicit ordering: Ordering[T]): Iterable[T] = {
    // binary search for one element takes O(logn) time, thus if n >> m, O(mlogn) << O(nlogm), where m is the size
    // of the shortest array
    (if (xs.size < ys.size) {
      xs
        .filter(x => binarySearch(ys, x).isDefined)
    } else if (xs.size > ys.size) {
      ys
        .filter(y => binarySearch(xs, y).isDefined)
    } else {
      xs
        .zip(ys)
        .filter(x => ordering.equiv(x._1, x._2))
        .map(_._1)
    })
      .toSet
  }

  /*
   * 13.5 Smallest nonconstructible value
   */
  def smallestNonconstructibleValue(xs: Seq[Int]): Option[Int] = {
    xs
      .sorted
      .iterator
      .scanLeft((-1, true)) { case ((previous, _), current) =>
        if (previous == -1) {
          (current, true)
        } else if (current - previous <= 1) {
          (previous + current, true)
        } else {
          (previous + 1, false)
        }
      }
      .dropWhile(_._2)
      .take(1)
      .map(_._1)
      .reduceOption(snd[Int])
  }

  /*
   * 13.7 Merging intervals
   */
  def mergeIntervals(xs: Seq[(Int, Int)], interval: (Int, Int)): Seq[(Int, Int)] = {
    def isOverlapping(i1: (Int, Int), i2: (Int, Int)): Boolean = {
      i2._2 >= i1._1 && i2._2 <= i1._2 || // i2 end overlaps
        i2._1 >= i1._1 && i2._1 <= i1._2 || // i2 start overlaps
        i1._2 >= i2._1 && i1._2 <= i2._2 || // i1 end overlaps
        i1._1 >= i2._1 && i1._1 <= i2._2 // i1 start overlaps
    }

    def merge1(i1: (Int, Int), i2: (Int, Int)): (Int, Int) = {
      (math.min(i1._1, i2._1), math.max(i1._2, i2._2))
    }

    def merge2(xs: Seq[(Int, Int)], tmp: (Int, Int), interval: (Int, Int)): Seq[(Int, Int)] = {
      xs match {
        case head +: tail =>
          if (isOverlapping(head, interval)) {
            merge2(tail, merge1(head, tmp), interval)
          } else {
            if (tmp == interval) {
              head +: merge2(tail, interval, interval)
            } else {
              tmp +: head +: merge2(tail, interval, interval)
            }
          }
        case _ => Seq.empty
      }
    }

    merge2(xs, interval, interval)
  }

  /*
   * 13.9 Partitioning and sorting an array with many repeated entries
   */
  def groupByAge(a: Array[(String, Int)]): Unit = {
    val freqMap = a
      .foldLeft(mutable.Map[Int, Int]()) { case (acc, (_, age)) =>
        acc(age) = acc.getOrElse(age, 0) + 1
        acc
      }

    val offsetMap = freqMap
      .foldLeft((mutable.Map[Int, Int](), 0)) { case ((map, previousCount), (age, count)) =>
        if (map.isEmpty) {
          map(age) = 0
        } else {
          map(age) = previousCount
        }
        (map, count + previousCount)
      }
      ._1

    def swap(i: Int, j: Int): Unit = {
      val tmp = a(i)
      a(i) = a(j)
      a(j) = tmp
    }

    /*
     * the idea is to move xs(offset) to its final position in the array. we first find the student at xs(offset),
     * get their age, find the rightful index from offset map, and then swap xs(offset) with the element in that index.
     * we also increment the corresponding offset, so that if there are more students of the same age,
     * they end up after the first one.
     * repeat with the element that was moved to offset.
     *
     * TODO: can we do it without mutating the maps, or with least mutation?
     */
    @tailrec
    def group(): Unit = {
      if (offsetMap.nonEmpty) {
        val (_, from) = offsetMap.head
        val age = a(from)._2
        val to = offsetMap(age)
        swap(from, to)
        freqMap(age) = freqMap(age) - 1
        if (freqMap(age) > 0) {
          offsetMap(age) = to + 1
        } else {
          offsetMap -= age
        }
        group()
      }
    }

    group()
  }
}
