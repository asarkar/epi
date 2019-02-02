package org.asarkar.epi.hashtables

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class HashTablesSpec extends FlatSpec {
  "hash tables" should "find the nearest repeated entries in an array" in {
    val text = List("All", "work", "and", "no", "play", "makes", "for",
      "no", "work", "no", "fun", "and", "no", "results")
    nearestRepetition(text) shouldBe 2
  }

  it should "find the smallest subarray" in {
    val paragraph1 = IndexedSeq("apple", "banana", "apple", "apple", "dog", "cat",
      "apple", "dog", "banana", "apple", "cat", "dog")
    val keywords1 = Set("banana", "cat")
    smallestSubarray(paragraph1, keywords1) shouldBe(8, 10)
    val paragraph2 = IndexedSeq("apple", "banana", "cat", "apple")
    val keywords2 = Set("banana", "apple")
    smallestSubarray(paragraph2, keywords2) shouldBe(0, 1)
  }

  it should "find the smallest sequential subarray" in {
    val paragraph1 = IndexedSeq("apple", "banana", "apple", "apple", "dog", "cat",
      "apple", "dog", "banana", "apple", "cat", "dog")
    val keywords1 = IndexedSeq("banana", "cat")
    smallestSeqSubarray(paragraph1, keywords1) shouldBe(8, 10)
    val paragraph2 = IndexedSeq("apple", "banana", "cat", "apple")
    val keywords2 = IndexedSeq("banana", "apple")
    smallestSeqSubarray(paragraph2, keywords2) shouldBe(1, 3)
  }

  it should "find the length of a longest containing interval" in {
    val xs1 = IndexedSeq(3, -2, 7, 9, 8, 1, 2, 0, -1, 5, 8)
    longestInterval(xs1) shouldBe 6
    val xs2 = IndexedSeq(10, 5, 3, 11, 6, 100, 4)
    longestInterval(xs2) shouldBe 4
  }
}
