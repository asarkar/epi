package org.asarkar.epi.stacks

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks

class StacksSpec extends FlatSpec with TableDrivenPropertyChecks {
  "stacks" should "test a string for well-formedness" in {
    val data = Table(
      ("expr", "wellFormed"),
      ("", true),
      ("([]){()}", true),
      ("[()[]{()()}]", true),
      ("{)", false),
      ("[()[]{()()", false)
    )

    forAll(data) { (expr, wellFormed) =>
      isWellFormed(expr) shouldBe wellFormed
    }
  }
}
