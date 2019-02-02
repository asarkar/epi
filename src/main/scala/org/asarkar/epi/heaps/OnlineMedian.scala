package org.asarkar.epi.heaps

import java.util.{PriorityQueue => JPq}

/*
 * 10.5 Compute the median of online data
 */
class OnlineMedian[T](implicit ordering: Ordering[T], n: Numeric[T]) {
  /*
   * Each heap contains about one half of the data. Every element in the min-heap is greater than or equal
   * to the median, and every element in the max-heap is less than or equal to the median.
   * The idea is to keep track of the "middle" 2 elements and then compute median among them. See test for examples.
   *
   * Scala mutable.PriorityQueue doesn't have a peek method, head doesn't honor the ordering
   */
  private val minHeap = new JPq[T]()
  private val maxHeap = new JPq[T](ordering.reverse)

  def insert(e: T): Unit = {
    minHeap.add(e)

    rebalance()
  }

  private def rebalance(): Unit = {
    maxHeap.add(minHeap.remove())
    if (maxHeap.size > minHeap.size) {
      minHeap.add(maxHeap.remove())
    }
  }

  def median: Double = {
    val min = n.toDouble(minHeap.peek)
    if (minHeap.size == maxHeap.size) {
      (min + n.toDouble(maxHeap.peek)) / 2
    } else {
      min
    }
  }
}