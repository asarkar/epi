package org.abhijitsarkar.stacks

import org.scalactic.Equality
import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor2}

class RpnEvaluatorSpec extends FlatSpec with TableDrivenPropertyChecks {
  "Evaluator" should "evaluate expressions" in {
    val rpn: TableFor2[String, Double] =
      Table(
        ("expression", "value"),
        ("6", 6),
        ("123", 123),
        ("-42", -42),
        ("3, 4, +, 2, x, 1, +", 15),
        ("1, 1, +, -2, x", -4),
        ("-641, 6, /, 28, /", -3.82),
        ("6, 4, -, 1, +", 3)
      )

    import org.scalactic.TolerantNumerics._

    implicit val dblEquality: Equality[Double] = tolerantDoubleEquality(0.10)
    forAll(rpn) { (expr: String, value: Double) =>
      new RpnEvaluator().evaluate(expr) === value
    }
  }
}
