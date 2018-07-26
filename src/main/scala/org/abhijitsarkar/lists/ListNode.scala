package org.abhijitsarkar.lists

import java.util.Objects

import org.abhijitsarkar.snd

class ListNode[T] private(val datum: T, var next: Option[ListNode[T]] = None)(implicit ordering: Ordering[T])
  extends Ordered[ListNode[T]] {
  def toSeq: Seq[T] = Iterator.iterate(Option(this))(_.flatMap(_.next))
    .takeWhile(_.isDefined)
    .flatten
    .map(_.datum)
    .toSeq

  def size: Int = Iterator.iterate(Option(this))(_.flatMap(_.next))
    .takeWhile(_.isDefined)
    .size

  def head: ListNode[T] = {
    this
  }

  def last: ListNode[T] = {
    Iterator.iterate(Option(this))(_.flatMap(_.next))
      .takeWhile(_.isDefined)
      .flatten
      .reduce(snd[ListNode[T]])
  }

  override def toString = s"$datum"

  override def equals(other: Any): Boolean = other match {
    case that: ListNode[T] => (that canEqual this) && compare(that) == 0
    case _ => false
  }

  override def compare(that: ListNode[T]): Int = ordering.compare(datum, that.datum)

  def canEqual(other: Any): Boolean = other.isInstanceOf[ListNode[T]]

  override def hashCode(): Int = {
    Objects.hashCode(datum)
  }
}

object ListNode {
  def apply[T](data: T*)(implicit ordering: Ordering[T]): ListNode[T] = {
    data.map(new ListNode[T](_))
      .reduceRight { (e1, e2) => e1.next = Some(e2); e1 }
  }

  def apply[T](datum: T, next: ListNode[T])(implicit ordering: Ordering[T]): ListNode[T] = {
    new ListNode[T](datum, Some(next))
  }
}
