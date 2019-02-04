package org.asarkar.epi.lists

sealed trait LinkedList[+A] {
  def datum: A = ???

  def next: LinkedList[A] = ???

  def isEmpty: Boolean = true

  def nonEmpty: Boolean = !isEmpty

  def foldLeft[B](acc: B)(f: (B, A) => B): B = acc

  def toSeq: Seq[A] = foldLeft(Seq.empty[A])((acc, x) => acc :+ x)
}

case object Nil extends LinkedList[Nothing]

case class Cons[+A](
                     override val datum: A,
                     override val next: LinkedList[A],
                   ) extends LinkedList[A] {
  override def isEmpty: Boolean = false

  override def foldLeft[B](acc: B)(f: (B, A) => B): B = {
    val current = f(acc, datum)
    next.foldLeft(current)(f)
  }
}

object LinkedList {
  def apply[A](xs: A*): LinkedList[A] = xs.foldRight(empty[A])((x, acc) => LinkedList(x, acc))

  def apply[A](datum: A, next: LinkedList[A]): LinkedList[A] = Cons(datum, next)

  def empty[A]: LinkedList[A] = Nil
}
