package org.abhijitsarkar.bintree

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor2}

class BinaryTreeNodeSpec extends FlatSpec with TableDrivenPropertyChecks {
  "BinaryTreeNode" should "compute height" in {
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

    val nodes: TableFor2[List[BinaryTreeNode[Char]], Int] =
      Table(
        ("nodes", "height"),
        (List(a), 4),
        (List(b), 3),
        (List(k, c), 2),
        (List(h, l, d), 1),
        (List(e, f, g, i, j, m, n, o), 0)
      )

    forAll(nodes) { (xs: List[BinaryTreeNode[Char]], height: Int) =>
      xs.foreach { x =>
        val h = x.height
        h shouldBe height
        x.isLeaf shouldBe h == 0
      }

    }
  }
}
