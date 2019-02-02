package org.asarkar.epi.stacks

import scala.collection.mutable

/*
 * 8.1 Implement a stack with max API
 *
 * https://stackoverflow.com/a/5753935/839733
 *
 * ListBuffer internally uses Nil and :: to build an immutable List and allows constant-time removal of the
 * first and last elements. To do so, it keeps a pointer on the first and last element of the list, and is
 * actually allowed to change the head and tail of the (otherwise immutable) :: class
 * (nice trick allowed by the private[scala] var members of ::). Its toList method returns the normal immutable List
 * in constant time as well, as it can directly return the structure maintained internally.
 * It is also the default builder for immutable Lists
 * (and thus can indeed be reasonably expected to have constant-time append).
 * If you call toList and then again append an element to the buffer, it takes linear time
 * with respect to the current number of elements in the buffer to recreate a new structure,
 * as it must not mutate the exported list any more.
 *
 * MutableList works internally with LinkedList instead, an (openly, not like ::) mutable linked list implementation
 * which knows of its element and successor (like ::). MutableList also keeps pointers to the first and last element,
 * but toList returns in linear time, as the resulting List is constructed from the LinkedList. Thus, it doesn't need
 * to reinitialize the buffer after a List has been exported.
 *
 * Given your requirements, I'd say ListBuffer and MutableList are equivalent. If you want to export their internal
 * list at some point, then ask yourself where you want the overhead: when you export the list, and then no overhead
 * if you go on mutating buffer (then go for MutableList), or only if you mutable the buffer again,
 * and none at export time (then go for ListBuffer).
 *
 * My guess is that in the 2.8 collection overhaul, MutableList predated ListBuffer and the whole Builder system.
 * Actually, MutableList is predominantly useful from within the collection.mutable package:
 * It has a private[mutable] def toLinkedList method which returns in constant time,
 * and can thus efficiently be used as a delegated builder for all structures that maintain a LinkedList internally.
 *
 * So I'd also recommend ListBuffer, as it may also get attention and optimization in the future than "purely mutable"
 * structures like MutableList and LinkedList.
 */

/*
 * https://stackoverflow.com/a/17598417/839733
 *
 * Ordering isn't contravariant. This is why I used Ordering[_ >: T], because it allows greater flexibility.
 * You can use an ordering that is defined for a superclass of T.
 */
class StackWithMax[T]()(implicit ordering: Ordering[_ >: T]) {
  private val buffer = mutable.ListBuffer[T]()
  private val maxBuffer = mutable.ListBuffer[(T, Int)]()

  def push(e: T): Unit = {
    debug(s"Before pushing $e")
    e +=: buffer
    insertMax(e)
    debug(s"After pushing $e")
  }

  private def insertMax(e: T): Unit = {
    if (maxBuffer.isEmpty || ordering.lt(peekMax, e)) {
      (e, 1) +=: maxBuffer
    } else if (ordering.equiv(peekMax, e)) {
      val max = maxBuffer.remove(0)
      (e, max._2 + 1) +=: maxBuffer
    }
  }

  def pop(): T = {
    debug("Before popping")
    val e = buffer.remove(0)
    if (isGteqMax(e)) {
      removeMax()
    }
    debug(s"After popping $e")
    e
  }

  private def isGteqMax(e: T): Boolean = {
    maxBuffer.isEmpty || ordering.gteq(e, peekMax)
  }

  private def peekMax: T = {
    maxBuffer.head._1
  }

  private def removeMax(): T = {
    val max = maxBuffer.remove(0)
    if (max._2 > 1) {
      (max._1, max._2 - 1) +=: maxBuffer
    }
    max._1
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

  def max: T = {
    peekMax
  }

  def isEmpty: Boolean = {
    buffer.isEmpty
  }

  def size: Int = {
    buffer.size
  }
}
