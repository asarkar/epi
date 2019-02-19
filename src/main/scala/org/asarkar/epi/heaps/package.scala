package org.asarkar.epi

import java.awt.geom.Point2D

import scala.collection.JavaConverters._
import scala.collection.mutable

package object heaps {
  /*
   * 10.2 Sort an increasing-decreasing array
   */
  def sortIncDec[T](a: List[T])(implicit ordering: Ordering[T]): List[T] = {
    val incDec = partitionIntoIncDec(a)

    def isInc(i: Int): Boolean = {
      i % 2 == 0
    }

    val pq = mutable.PriorityQueue[T]()(ordering.reverse)

    Iterator.from(0)
      .map { i =>
        incDec
          .zipWithIndex
          .foreach { case ((start, end), j) =>
            /*
             * add the smallest unprocessed element from each group to the heap.
             * contrary to what the book says, there's no need for reversing the decreasing groups, just process
             * from the end instead of the beginning.
             */
            val k = if (isInc(j)) {
              start + i
            } else {
              end - i
            }

            if (k >= start && k <= end) {
              pq += a(k)
            }
          }
        pq.dequeue()
      }
      .drop(1) // ignore 0
      .takeWhile(_ => pq.nonEmpty)
      .toList
  }

  def partitionIntoIncDec[T](a: List[T])(implicit ordering: Ordering[T]): List[(Int, Int)] = {
    def lastIndexWhere(start: Int, predicate: (T, T) => Boolean): Int = {
      Iterator.from(start + 1)
        .takeWhile(a.isDefinedAt)
        .dropWhile(x => predicate(a(x - 1), a(x)))
        .take(1)
        .foldLeft(a.size)(snd[Int])
    }

    val it = Iterator.iterate((0, true)) { case (end, increasing) =>
      (lastIndexWhere(end, if (increasing) ordering.lt else ordering.gt), !increasing)
    }
      .drop(1) // 0 isn't an end
      .map(_._1)
      .takeWhile(a.isDefinedAt)
      .flatMap(x => List(x - 1, x))

    (Iterator.single(0) ++ it ++ Iterator.single(a.size - 1))
      .sliding(2, 2)
      .map { x =>
        assert(x.size == 2)
        (x.head, x.last)
      }
      // resolve fully, iterator can't be reused
      .toList
  }

  /*
   * 10.4 Compute the k closest stars
   *
   * ANSWER: We keep a max heap of size k + 1, and whenever the heap size exceeds k, we remove the top element (max).
   * This way, we can handle an unlimited number of input elements because we only need O(k) space for the heap.
   * Time taken is O(n) for iteration + O(n log(k)) for insertion in the heap.
   */
  def kClosest(points: Seq[(Int, Int)], k: Int): Seq[(Int, Int)] = {
    val origin = new Point2D.Double(0, 0)
    val ord = Ordering.by[Point2D, Double](_.distanceSq(origin)).reverse
    val maxHeap = new java.util.PriorityQueue[Point2D.Double](k + 1, ord)

    points
      .map(p => new Point2D.Double(p._1, p._2))
      .foreach { p =>
        maxHeap.offer(p)
        if (maxHeap.size > k) {
          maxHeap.poll()
        }
      }

    assert(maxHeap.size == k, s"Min heap size: ${maxHeap.size} is not equal to: $k")

    maxHeap
      .iterator
      .asScala
      .map(p => (p.getX.intValue, p.getY.intValue))
      .toList
  }
}
