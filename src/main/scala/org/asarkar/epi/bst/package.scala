package org.asarkar.epi

import org.asarkar.epi.bintree.BinaryTreeNode

package object bst {
  /*
   * 14.1 Test if a binary tree satisfies the BST property
   */
  def isBST[T](root: BinaryTreeNode[T])(implicit ordering: Ordering[T]): Boolean = {
    (root.left, root.right) match {
      case (Some(l), Some(r)) => ordering.lteq(l.datum, root.datum) &&
        ordering.gteq(r.datum, root.datum) &&
        isBST(l) &&
        isBST(r)
      case (Some(l), None) => ordering.lteq(l.datum, root.datum) && isBST(l)
      case (None, Some(r)) => ordering.gteq(r.datum, root.datum) && isBST(r)
      case _ => true
    }
  }
}
