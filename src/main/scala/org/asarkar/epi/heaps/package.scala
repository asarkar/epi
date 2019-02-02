package org.asarkar.epi

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
}
