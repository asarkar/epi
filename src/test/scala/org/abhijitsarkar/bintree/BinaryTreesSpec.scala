package org.abhijitsarkar.bintree

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.OptionValues._

class BinaryTreesSpec extends FlatSpec {
  "Binary tree" should "test if it's height-balanced" in {
    val e = new BinaryTreeNode[Char]('E')
    val f = new BinaryTreeNode[Char]('F')
    val d = new BinaryTreeNode[Char]('D')
    val g = new BinaryTreeNode[Char]('G')
    val i = new BinaryTreeNode[Char]('I')
    val j = new BinaryTreeNode[Char]('J')
    val m = new BinaryTreeNode[Char]('M')
    val n = new BinaryTreeNode[Char]('N')
    val c = new BinaryTreeNode[Char]('C')
    val h = new BinaryTreeNode[Char]('H')
    val l = new BinaryTreeNode[Char]('L')
    val o = new BinaryTreeNode[Char]('O')
    val b = new BinaryTreeNode[Char]('B')
    val k = new BinaryTreeNode[Char]('K')
    val a = new BinaryTreeNode[Char]('A')

    d.left = Some(e)
    d.right = Some(f)
    c.left = Some(d)
    c.right = Some(g)
    h.left = Some(i)
    h.right = Some(j)
    l.left = Some(m)
    l.right = Some(n)
    b.left = Some(c)
    b.right = Some(h)
    k.left = Some(l)
    k.right = Some(o)
    a.left = Some(b)
    a.right = Some(k)

    isHeightBalanced(a) shouldBe true
  }

  it should "test if a binary tree is symmetric" in {
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

    isSymmetric(`314`) shouldBe true

    val `561right` = new BinaryTreeNode[Int](561)
    `561right`.right = Some(`3right`)
    `6left`.right = Some(`561right`)

    isSymmetric(`314`) shouldBe false

    val `561left` = new BinaryTreeNode[Int](561)
    `6right`.left = Some(`561left`)

    isSymmetric(`314`) shouldBe false
  }

  it should "compute the lca" in {
    val nine = new BinaryTreeNode[Int](9)
    val five = new BinaryTreeNode[Int](5)
    val eleven = new BinaryTreeNode[Int](11)
    eleven.left = Some(nine)
    eleven.right = Some(five)
    val two = new BinaryTreeNode[Int](2)
    val six = new BinaryTreeNode[Int](6)
    six.left = Some(two)
    six.right = Some(eleven)
    val seven = new BinaryTreeNode[Int](7)
    val thireteen = new BinaryTreeNode[Int](13)
    thireteen.left = Some(seven)
    val eight = new BinaryTreeNode[Int](8)
    eight.right = Some(thireteen)
    val three = new BinaryTreeNode[Int](3)
    three.left = Some(six)
    three.right = Some(eight)

    lca(three, 2, 5).value shouldBe 6
  }

  it should "find a root to leaf path with specified sum" in {
    val five = new BinaryTreeNode[Int](5)
    val three = new BinaryTreeNode[Int](3)
    five.left = Some(three)
    val one = new BinaryTreeNode[Int](1)
    one.left = Some(five)
    val seven = new BinaryTreeNode[Int](7)
    one.right = Some(seven)

    hasPathWithSum(one, 9) shouldBe true
    hasPathWithSum(one, 8) shouldBe true
    hasPathWithSum(one, 10) shouldBe false
  }

  it should "traverse the tree inorder" in {
    val one = new BinaryTreeNode[Int](1)
    val two = new BinaryTreeNode[Int](2)
    val three = new BinaryTreeNode[Int](3)
    val four = new BinaryTreeNode[Int](4)
    val five = new BinaryTreeNode[Int](5)

    two.left = Some(four)
    two.right = Some(five)
    one.left = Some(two)
    one.right = Some(three)

    inorder(one) should contain only(4, 2, 5, 1, 3)
  }

  it should "traverse the tree preorder" in {
    val one = new BinaryTreeNode[Int](1)
    val two = new BinaryTreeNode[Int](2)
    val three = new BinaryTreeNode[Int](3)
    val four = new BinaryTreeNode[Int](4)
    val five = new BinaryTreeNode[Int](5)

    two.left = Some(four)
    two.right = Some(five)
    one.left = Some(two)
    one.right = Some(three)

    inorder(one) should contain only(1, 2, 4, 5, 3)
  }

  type Pabtn = ParentAwareBinaryTreeNode[Char]
  it should "compute the inorder successor" in {
    val a = new Pabtn('A')
    // left subtree
    val b = new Pabtn('B', parent = Some(a))
    val i = new Pabtn('I', parent = Some(a))
    a._left = Some(b)
    a._right = Some(i)

    val c = new Pabtn('C', parent = Some(b))
    val f = new Pabtn('F', parent = Some(b))
    b._left = Some(c)
    b._right = Some(f)

    val d = new Pabtn('D', parent = Some(c))
    val e = new Pabtn('E', parent = Some(c))
    c._left = Some(d)
    c._right = Some(e)

    val g = new Pabtn('G', parent = Some(f))
    f._right = Some(g)

    val h = new Pabtn('H', parent = Some(g))
    g._left = Some(h)

    // right subtree
    val j = new Pabtn('J', parent = Some(i))
    val o = new Pabtn('O', parent = Some(i))
    i._left = Some(j)
    i._right = Some(o)

    val k = new Pabtn('K', parent = Some(j))
    j._right = Some(k)

    val p = new Pabtn('P', parent = Some(o))
    o._right = Some(p)

    val l = new Pabtn('L', parent = Some(k))
    val n = new Pabtn('N', parent = Some(k))
    k._left = Some(l)
    k._right = Some(n)

    val m = new Pabtn('M', parent = Some(l))
    l._right = Some(m)

    inorderSuccessor(b).value shouldBe 'F'
    inorderSuccessor(h).value shouldBe 'G'
    inorderSuccessor(g).value shouldBe 'A'
    inorderSuccessor(j).value shouldBe 'L'
    inorderSuccessor(p) shouldBe empty
  }
}
