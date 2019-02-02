package org.asarkar.epi.heaps

import org.scalactic.Equality
import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor2}

class OnlineMedianSpec extends FlatSpec with TableDrivenPropertyChecks {
  "OnlineMedian" should "compute the median" in {
    val data: TableFor2[Int, Double] =
      Table(
        ("element", "median"), // hypothetical sorted sequence shown in comments
        // (1)
        // min(1)
        (1, 1),
        // (0, 1)
        // 0 is less than current median 0, goes to max heap which contains all elements to the left of the median
        // min (1) max (0)
        (0, 0.5d),
        // (0, 1, 3)
        // 3 is greater than current median 1, goes to min heap, which contains all elements to the right of the median
        // min(1,3) max(0)
        (3, 1),
        // (0, 1, 3, 5)
        // 5 goes to min heap
        // step 1 min (1,3,5) max (0)
        // step 2 rebalance min (3,5) max (0,1)
        (5, 2),
        // (0, 1, 2, 3, 5)
        (2, 2),
        // (0, 0, 1, 2, 3, 5)
        (0, 1.5d),
        // (0, 0, 1, 1, 2, 3, 5)
        (1, 1)
      )

    import org.scalactic.TolerantNumerics._

    implicit val dblEquality: Equality[Double] = tolerantDoubleEquality(0.10)

    val onlineMedian = new OnlineMedian[Int]()
    forAll(data) { (e: Int, median: Double) =>
      onlineMedian.insert(e)
      onlineMedian.median shouldBe median
    }
  }
}
