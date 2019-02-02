package org.asarkar.epi

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random

package object recursion {
  /*
   * 15.1 The Towers of Hanoi problem
   */
  def hanoi(n: Int): Seq[(Int, Int)] = {
    val sum = (0 until n).sum
    val pegs = Array.fill[Seq[Int]](3)(Seq.empty)
    pegs(0) = 1 to n

    def move(from: Int, to: Int, numRings: Int): Seq[(Int, Int)] = {
      numRings match {
        case 0 => Seq.empty
        case 1 =>
          pegs(from) match {
            case head +: tail =>
              assert(pegs(to).isEmpty || head < pegs(to).head, "Cannot place a smaller ring above a larger ring")
              pegs(from) = tail
              pegs(to) = head +: pegs(to)
              Seq((from, to))
          }
        case _ =>
          val spare = sum - from - to
          move(from, spare, numRings - 1) ++
            move(from, to, 1) ++
            move(spare, to, numRings - 1)
      }
    }

    val dest = Iterator.continually {
      Random.nextInt(3)
    }
      .dropWhile(_ == 0)
      .take(1)
      .next()
    val moves = move(0, dest, 3)
    val xs = pegs(dest)
    assert(xs.size == n, s"Destination peg $dest doesn't contain all $n rings")

    moves
  }

  /*
   * 15.2 Generate all non-attacking placements of n-Queens
   */
  def nQueen(n: Int): Seq[IndexedSeq[Int]] = {
    val board = mutable.Buffer[(Int, Int)]()

    def isUnderAttack(current: (Int, Int)): Boolean = {
      board
        .exists { previous =>
          current._2 == previous._2 || // vertical attack
            current._1 + current._2 == previous._1 + previous._2 || // diagonal attack
            current._1 - current._2 == previous._1 - previous._2 // diagonal attack
        }
    }

    def flatten(board: Seq[(Int, Int)]): IndexedSeq[Int] = {
      board
        .map(_._2)
        .toIndexedSeq
    }

    def tryPlaceQueen(previous: (Int, Int)): Seq[IndexedSeq[Int]] = {
      val (previousRow, _) = previous
      if (previousRow >= n - 1) {
        Seq(flatten(board))
      } else {
        (0 until n)
          .map((previousRow + 1, _))
          .filterNot(isUnderAttack)
          .flatMap { position =>
            board += position
            val brd = tryPlaceQueen(position)
            board -= position
            brd
          }
      }
    }

    tryPlaceQueen((-1, -2))
  }

  /*
   * 15.3 Generate permutations
   */
  def permutations(xs: mutable.IndexedSeq[Int]): Seq[Seq[Int]] = {
    def swap(x: Int, y: Int): Unit = {
      if (x != y) {
        val tmp = xs(x)
        xs(x) = xs(y)
        xs(y) = tmp
      }
    }

    def permute(i: Int): Seq[Seq[Int]] = {
      if (i < xs.size - 1) {
        (i until xs.size)
          .flatMap { j =>
            swap(i, j)
            val tmp = permute(i + 1)
            swap(j, i)
            tmp
          }
      } else {
        // clone to capture current state; swapped elements are restored when recursive call returns
        Seq(xs.clone())
      }
    }

    permute(0)
  }

  /*
   * 15.4 Generate the power set
   *
   * ANSWER: We use the subroutine for generating subsets of size k for k in [0, n].   *
   * Note that the subsets are generated in the gray code order, i.e. each subset differs from the previous or next one
   * by a single element.
   */
  def powerSet1(n: Int): Seq[Seq[Int]] = {
    (0 to n)
      .flatMap(subsets(n, _))
  }

  /*
   * Alternative implementation of 15.4
   * We generate gray codes of size n, and generate the subsets corresponding to the bits set to 1.
   */
  def powerSet2(n: Int): Seq[Seq[Int]] = {
    val xs = 1 to n

    grayCode(n)
      .map(code => code.zipWithIndex.filter(_._1 == 1).map(x => xs(x._2)))
  }

  /*
   * 1. Start with the gray codes for 1-bit, i.e. 0 and 1.
   * 2. Create a mirror image of the existing gray codes (flip each bit).
   * 3. Prefix the original values with 0 and the mirrored values with 1.
   * 4. Repeat steps (2) and (3) until the desired width is achieved.
   */
  def grayCode(n: Int): Seq[Seq[Byte]] = {
    val x = 1 << n
    Iterator.iterate(Seq(ListBuffer(0.byteValue()), ListBuffer(1.byteValue()))) { prev =>
      prev
        .map(0.byteValue() +: _.map(_.byteValue())) ++
        prev
          .map(code => 1.byteValue() +: code.map(b => (b ^ 1).byteValue()))
    }
      .dropWhile(_.size < x)
      .take(1)
      .flatten
      .toList
  }

  /*
   * 15.5 Generate all subsets of size k
   *
   * ANSWER: Each element may or may not be included in a subset. When it's included, we take one fewer element
   * from the remaining elements. When it's not included, we take the same number of elements from the remaining
   * elements.
   * The base cases are there are more elements to take than remaining, or when no elements are to be taken.
   */
  def subsets(n: Int, k: Int): Seq[Seq[Int]] = {
    if (k <= 0 || k > n) Seq(Seq.empty[Int])
    else {
      val xs = subsets(n - 1, k - 1)
        .map(n +: _)
      val ys = subsets(n - 1, k)
        .filter(_.nonEmpty)
      xs ++ ys
    }
  }
}
