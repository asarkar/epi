package org.asarkar.epi.bintree

import java.util.Objects

class BinaryTreeNode[T](
                         val datum: T,
                         var left: Option[BinaryTreeNode[T]] = None,
                         var right: Option[BinaryTreeNode[T]] = None
                       )(implicit ordering: Ordering[T]) extends Ordered[BinaryTreeNode[T]] {
  def root: BinaryTreeNode[T] = {
    this
  }

  def isLeaf: Boolean = {
    List(left, right).flatten.isEmpty
  }

  def height: Int = {
    val height = if (isLeaf) {
      0
    } else {
      List(left.map(_.height + 1), right.map(_.height + 1))
        .flatten
        .max
    }
    height
  }

  override def toString = s"$datum"

  override def equals(other: Any): Boolean = other match {
    case that: BinaryTreeNode[T] => (that canEqual this) && compare(that) == 0
    case _ => false
  }

  override def compare(that: BinaryTreeNode[T]): Int = ordering.compare(datum, that.datum)

  /*
   * https://stackoverflow.com/a/32094143/839733
   *
   * The canEquals method is used to cover the expectation that equals should be symmetric - that is,
   * if (and only if) a.equals(b) is true, then b.equals(a) should also be true.
   * Problems with this can arise when comparing an instance of a class with an instance of a sub-class.
   * Eg.
   * class Animal(numLegs: Int, isCarnivore: Boolean) {
   *    def equals(other: Any) = other match {
   *      case that: Animal =>
   *        this.numLegs == that.numLegs &&
   *        this.isCarnivore == that.isCarnivore
   *      case _ => false
   *    }
   *  }
   *
   *  class Dog(numLegs: Int, isCarnivore: Boolean, breed: String) extends Animal(numLegs, isCarnivore) {
   *    def equals(other: Any) = other match {
   *      case that: Dog =>
   *       this.numLegs == that.numLegs &&
   *       this.isCarnivore == that.isCarnivore &&
   *       this.breed == that.breed
   *      case _ => false
   *    }
   *  }
   *
   *  val cecil = new Animal(4, true)
   *  val bruce = new Dog(4, true, "Boxer")
   *  cecil.equals(bruce) // true
   *  bruce.equals(cecil) // false - cecil isn't a Dog!
   *
   *  To fix this, ensure the two entities are of the same (sub-)type using canEqual
   */
  def canEqual(other: Any): Boolean = other.isInstanceOf[BinaryTreeNode[T]]

  override def hashCode(): Int = {
    Objects.hashCode(datum)
  }
}
