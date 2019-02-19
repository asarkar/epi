package org.asarkar.epi

import org.scalactic.Equality

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

package object bintree {
  /*
   * 9.1 Test if a binary tree is height-balanced
   */
  def isHeightBalanced[T](root: BinaryTreeNode[T]): Boolean = {
    def checkBalanced(node: BinaryTreeNode[T]): (Int, Boolean) = {
      val o = for {
        left <- node.left.map(checkBalanced) if left._2
        // if left subtree is unbalanced, the following is short-circuited
        right <- node.right.map(checkBalanced) if right._2
        balanced = math.abs(left._1 - right._1) <= 1
        height = math.max(left._1 + 1, right._1 + 1)
      } yield (height, balanced)

      o.getOrElse((0, true))
    }

    checkBalanced(root)._2
  }

  /*
   * 9.2 Test if a binary tree is symmetric
   */
  def isSymmetric[T](root: BinaryTreeNode[T]): Boolean = {
    def isSymmetric(left: BinaryTreeNode[T], right: BinaryTreeNode[T]): Boolean = {
      List(left.left, right.right, left.right, right.left)
        .sliding(2, 2)
        .foldLeft(true) { case (symmetric, xs) =>
          symmetric && (xs.flatten match {
            case Nil => true
            case x :: y :: Nil => x == y && isSymmetric(x, y)
            case _ => false
          })
        }
    }

    root.left.zip(root.right).exists {
      Function.tupled(isSymmetric)
    }
  }

  /*
   * 9.3 Compute the LCA in a binary tree
   */
  def lca[T](root: BinaryTreeNode[T], value1: T, value2: T): Option[T] = {
    def isOneOfTheValues(value: T): Boolean = {
      List(value1, value2).contains(value)
    }

    /*
     * Check each node against the given values; if match, return the node to it's parent. A parent that receives
     * non-empty values from both left and right is the LCA.
     * https://www.youtube.com/watch?v=13m9ZCB8gjw
     */
    def lca(node: BinaryTreeNode[T]): Option[T] = {
      if (isOneOfTheValues(node.datum)) {
        Some(node.datum)
      } else {
        node.left.flatMap(lca) match {
          // found one of the given values on the left, search right for the other value
          case Some(x) if isOneOfTheValues(x) => node.right.flatMap(lca)
            .filter(isOneOfTheValues)
            .map(_ => node.datum)
          // not found on the left
          case None => node.right.flatMap(lca)
          // not one of the two values, must have found the lca on the left
          case x@_ => x
        }
      }
    }

    lca(root)
  }

  import org.scalactic.TolerantNumerics._

  private implicit val dblEquality: Equality[Double] = tolerantDoubleEquality(0.10)

  /*
   * 9.6 Find a root to leaf path with specified sum
   */
  def hasPathWithSum[T](root: BinaryTreeNode[T], value: Double)(implicit n: Numeric[T]): Boolean = {
    if (root.isLeaf) {
      value == root.datum
    } else {
      val diff = value - n.toDouble(root.datum)
      root.left.exists(x => hasPathWithSum(x, diff)) ||
        root.right.exists(x => hasPathWithSum(x, diff))
    }
  }

  /*
   * 9.7 Implement an inorder traversal without recursion
   */
  def inorder[T](root: BinaryTreeNode[T]): Iterable[T] = {
    val stack = ListBuffer[BinaryTreeNode[T]](root)
    val inorder = ListBuffer[T]()
    val visited = mutable.HashSet[BinaryTreeNode[T]]()

    Iterator.continually {
      val node = stack.remove(0)
      if (visited.contains(node)) {
        inorder += node.datum
        node.right.foreach(_ +=: stack)
      } else {
        node +=: stack
        node.left.foreach(_ +=: stack)
        visited += node
      }
      inorder
    }
      .takeWhile(_ => stack.nonEmpty)
      .reduce(snd[ListBuffer[T]])
  }

  /*
   * 9.8 Implement a preorder traversal without recursion
   */
  def preorder[T](root: BinaryTreeNode[T]): Iterable[T] = {
    val stack = ListBuffer[BinaryTreeNode[T]](root)

    Iterator.continually {
      val node = stack.remove(0)
      node.right.foreach(_ +=: stack)
      node.left.foreach(_ +=: stack)
      node.datum
    }
      .takeWhile(_ => stack.nonEmpty)
      .toList
  }

  /*
   * 9.10 Compute inorder successor
   */
  def inorderSuccessor[T](node: ParentAwareBinaryTreeNode[T]): Option[T] = {
    def leftmostChild(x: ParentAwareBinaryTreeNode[T]) = {
      Iterator.iterate((Option(x), false)) {
        /*
         * if left subtree present, keep going. otherwise, this is the leftmost node in the tree
         */
        case (a@Some(y), false) => y._left match {
          case b@Some(_) => (b, false)
          case _ => (a, true)
        }
        case x@_ => x
      }
    }

    def isLeftChild(c: ParentAwareBinaryTreeNode[T]): Boolean = {
      c.parent.flatMap(_._left).exists(_.datum == c.datum)
    }

    (node._right match {
      /*
       * if right subtree is present, successor is the leftmost node in it.
       * otherwise, keep moving up until we move up from a left subtree.
       */
      case Some(x) => leftmostChild(x)
      case _ => Iterator.iterate((Option(node), false)) {
        case (Some(child), false) => (child.parent, isLeftChild(child))
        case x@_ => x
      }
    })
      .takeWhile(_._1.isDefined)
      .dropWhile(!_._2)
      .take(1)
      .flatMap(_._1.map(_.datum))
      .reduceOption(snd[T])
  }

  class ParentAwareBinaryTreeNode[T](
                                      override val datum: T,
                                      // cannot override mutable superclass variable but won't compile without override
                                      // either if names are same
                                      var _left: Option[ParentAwareBinaryTreeNode[T]] = None,
                                      var _right: Option[ParentAwareBinaryTreeNode[T]] = None,
                                      var parent: Option[ParentAwareBinaryTreeNode[T]] = None
                                    )(implicit ordering: Ordering[T]) extends BinaryTreeNode[T](datum, _left, _right)

}
