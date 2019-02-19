package org.asarkar.epi.bst

import org.asarkar.epi.bintree.BinaryTreeNode
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class BSTSpec extends FlatSpec {
  "bst" should "test if a binary tree satisfies the BST property" in {
    val two = new BinaryTreeNode[Int](2)
    val five = new BinaryTreeNode[Int](5)
    val three = new BinaryTreeNode[Int](3, Some(two), Some(five))
    val thirteen = new BinaryTreeNode[Int](13)
    val seventeen = new BinaryTreeNode[Int](17, left = Some(thirteen))
    val eleven = new BinaryTreeNode[Int](11, right = Some(seventeen))
    val seven = new BinaryTreeNode[Int](7, Some(three), Some(eleven))

    isBST(seven) shouldBe true

    val `314` = new BinaryTreeNode[Int](314)
    val `6left` = new BinaryTreeNode[Int](6)
    val `6right` = new BinaryTreeNode[Int](6)
    val `2right` = new BinaryTreeNode[Int](2)
    val `2left` = new BinaryTreeNode[Int](2)
    val `3right` = new BinaryTreeNode[Int](3)
    val `3left` = new BinaryTreeNode[Int](3)

    `2right`.right = Some(`3right`)
    `6left`.right = Some(`2right`)
    `6right`.left = Some(`2left`)
    `2left`.left = Some(`3left`)
    `314`.left = Some(`6left`)
    `314`.right = Some(`6right`)

    isBST(`314`) shouldBe false
  }
}
