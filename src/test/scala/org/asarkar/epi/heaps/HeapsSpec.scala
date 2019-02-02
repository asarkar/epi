package org.asarkar.epi.heaps

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class HeapsSpec extends FlatSpec {
  "heaps" should "partition a list into increasing-decreasing seq" in {
    val a = List(57, 131, 493, 294, 221, 339, 418, 452, 442, 190)
    partitionIntoIncDec(a) should contain only((0, 2), (3, 4), (5, 7), (8, 9))
  }

  it should "sort an increasing-decreasing array" in {
    val a = List(57, 131, 493, 294, 221, 339, 418, 452, 442, 190)
    sortIncDec(a) should contain only(131, 190, 221, 294, 339, 418, 442, 452)
  }
}
