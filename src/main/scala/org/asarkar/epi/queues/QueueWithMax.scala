package org.asarkar.epi.queues

import scala.collection.JavaConverters._
import scala.collection.mutable

/*
 * 8.9 Implement a queue with max API
 */
class QueueWithMax[T]()(implicit ordering: Ordering[_ >: T]) {
  private val buffer = mutable.ListBuffer[T]()
  /*
   * Scala deprecated DoubleLinkedList with no replacement that offers constant time addition/removal from both ends
   */
  private var maxBuffer = new java.util.LinkedList[T]().asScala

  def enqueue(e: T): Unit = {
    debug(s"Before enqueueing $e")
    buffer += e
    insertMax(e)
    debug(s"After enqueueing $e")
  }

  private def insertMax(e: T): Unit = {
    Iterator.from(1)
      .takeWhile(_ => maxBuffer.nonEmpty && ordering.lt(maxBuffer.last, e))
      .foreach(_ => maxBuffer.remove(maxBuffer.size - 1))
    maxBuffer += e
  }

  def dequeue(): T = {
    debug("Before dequeueing")
    val e = buffer.remove(0)
    removeMax(e)
    debug(s"After dequeueing $e")
    e
  }

  private def debug(msg: String): Unit = {
    println(msg)
    println("---")
    println(s"buffer.size = ${buffer.size}")
    println(s"maxBuffer.size = ${maxBuffer.size}")
    println(s"buffer = $buffer")
    println(s"maxBuffer = $maxBuffer")
    println()
  }

  private def removeMax(e: T): Unit = {
    if (maxBuffer.nonEmpty && ordering.equiv(peekMax, e)) {
      maxBuffer.remove(0)
    }
  }

  def max: T = {
    peekMax
  }

  private def peekMax: T = {
    maxBuffer.head
  }

  def isEmpty: Boolean = {
    buffer.isEmpty
  }

  def size: Int = {
    buffer.size
  }
}


