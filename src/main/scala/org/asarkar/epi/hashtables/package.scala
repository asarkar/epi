package org.asarkar.epi

import scala.annotation.tailrec
import scala.collection.mutable

package object hashtables {
  /*
   * 12.5 Find the nearest repeated entries in an array
   */
  def nearestRepetition(text: List[String]): Int = {
    text
      .zipWithIndex
      .foldLeft((mutable.Map[String, Int](), text.length + 1)) { case ((m, nearest), (s, i)) =>
        val j = m.getOrElseUpdate(s, {
          i
        })
        val n = if (i > j) {
          m(s) = i
          math.min(nearest, i - j)
        } else {
          nearest
        }
        (m, n)
      }
      ._2
  }

  /*
   * 12.6 Find the smallest subarray covering all values
   */
  def smallestSubarray(paragraph: IndexedSeq[String], keywords: Set[String]): (Int, Int) = {
    val set = mutable.HashSet[String]()

    @tailrec
    def end(start: Int): Int = {
      if (!paragraph.isDefinedAt(start)) {
        return -1
      }

      val word = paragraph(start)
      if (keywords.contains(word)) {
        set += word
      }

      if (set.size == keywords.size) {
        start
      } else {
        end(start + 1)
      }
    }

    val i = end(0)

    Iterator.iterate((0, i, (0, i))) { case (start, nd, (min, max)) =>
      set -= paragraph(start)
      if (set.size == keywords.size) {
        (start + 1, nd, (start + 1, nd))
      } else {
        val newEnd = end(nd + 1)
        if (newEnd > start && max - min > newEnd - start) {
          (start + 1, newEnd, (start + 1, newEnd))
        } else {
          (start + 1, newEnd, (min, max))
        }
      }
    }
      .dropWhile(x => paragraph.isDefinedAt(x._1) && paragraph.isDefinedAt(x._2))
      .take(1)
      .map(_._3)
      .next()
  }

  /*
   * 12.7 Find the smallest subarray sequentially covering all values
   */
  def smallestSeqSubarray(paragraph: IndexedSeq[String], keywords: IndexedSeq[String]): (Int, Int) = {
    val indexMap = keywords
      .zipWithIndex
      .toMap
    val latest = Array.fill[Int](keywords.size)(Int.MaxValue)

    Iterator.iterate((0, 0, Int.MaxValue)) { case (i, min, max) =>
      val word = paragraph(i)
      indexMap.get(word)
        .map { j =>
          latest(j) = i
          if (j == keywords.size - 1 && latest.last > latest.head && max - min > latest.last - latest.head) {
            (i + 1, latest.head, latest.last)
          } else {
            (i + 1, min, max)
          }
        }
        .getOrElse((i + 1, min, max))
    }
      .dropWhile(x => paragraph.isDefinedAt(x._1))
      .take(1)
      .map(x => (x._2, x._3))
      .filter(_._2 < paragraph.size)
      .reduceOption(snd[(Int, Int)])
      .getOrElse((-1, -1))
  }

  /*
   * 12.9 Find the length of a longest contained interval
   */
  def longestInterval(xs: IndexedSeq[Int]): Int = {
    val set = mutable.HashSet(xs: _*)

    def scanAndRemove(it: Iterator[Int]): Int = {
      it
        .takeWhile(set.remove)
        .count(_ => true)
    }

    Iterator.from(0)
      .takeWhile(i => xs.isDefinedAt(i) && set.nonEmpty)
      .map { i =>
        val x = xs(i)
        // num items <= x
        val min = scanAndRemove(Iterator.from(x, -1))
        // // num items > x
        val max = scanAndRemove(Iterator.from(x + 1))
        min + max
      }
      .max
  }
}
