package org.asarkar.epi.honors

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks

class HonorsSpec extends FlatSpec with TableDrivenPropertyChecks {
  "honors" should "determine whether there is a possible arbitrage" in {
    val data = Table(
      ("rates", "arbitrage"),
      (IndexedSeq(
        IndexedSeq(1d, 0.7d, 13.57d),
        IndexedSeq(1.43d, 1d, 9.5d),
        IndexedSeq(0.16d, 0.11d, 1d)
      ), true)
    )

    forAll(data) { (rates, arbitrage) =>
      println(isArbitragePossible(rates))
      isArbitragePossible(rates) shouldBe arbitrage
    }
  }

  it should "find the first missing positive number" in {
    firstMissingPositiveNumber(Array(3, 4, -1, 1)) shouldBe 2
    firstMissingPositiveNumber(Array(1, 2, 0)) shouldBe 3
    firstMissingPositiveNumber(Array(-1, -2, -3)) shouldBe 1
    firstMissingPositiveNumber(Array(-1, -2, 0)) shouldBe 1
  }

  it should "compute how many units of water remain trapped on the map" in {
    val data = Table(
      ("map", "water"),
      (IndexedSeq(2, 1, 2), 1),
      (IndexedSeq(3, 0, 1, 3, 0, 5), 8),
      (IndexedSeq(7, 1, 4, 0, 2, 8, 6, 3, 5), 23)
    )
    forAll(data) { (map, water) =>
      rainWaterTrapped(map) shouldBe water
    }
  }


}
