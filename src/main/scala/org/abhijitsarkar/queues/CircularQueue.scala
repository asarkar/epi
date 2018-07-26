package org.abhijitsarkar.queues

import scala.reflect.ClassTag

/*
 * 8.7 Implement a Circular queue
 */
class CircularQueue[T: ClassTag] {
  private var _capacity: Int = 0

  private var buffer: Array[T] = _
  private var head: Int = 0
  private var tail: Int = 0
  private var _size: Int = 0

  def this(capacity: Int) {
    this()
    require(capacity > 0, "capacity must be positive")
    _capacity = capacity
    buffer = new Array[T](_capacity)
  }

  def size = _size

  def size_=(value: Int): Unit = _size = value

  def enqueue(e: T): Unit = {
    buffer(tail % _capacity) = e
    tail += 1
    _size += 1
  }

  def dequeue(): Option[T] = {
    if (isEmpty) {
      None
    } else {
      val e = buffer(head % _capacity)
      head += 1
      _size -= 1
      Some(e)
    }
  }

  def isEmpty: Boolean = {
    _size == 0
  }
}
