package org.asarkar.epi.stacks

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/*
 * 8.2 Evaluate RPN expressions
 */
class RpnEvaluator {

  def evaluate(expr: String): Double = {
    def parseInt(d: Int): (Int, Int) = {
      Iterator.from(d)
        .takeWhile(i => expr.isDefinedAt(i) && expr(i).isDigit)
        .map(expr(_).asDigit)
        .foldRight((0, 0)) { case (digit, (num, i)) => (num + math.pow(10, i).toInt * digit, i + 1) }
    }

    def isUnaryMinus(d: Int): Boolean = {
      Operator.isMinus(expr(d)) && (expr.isDefinedAt(d + 1) && expr(d + 1).isDigit)
    }

    @tailrec
    def evaluate(d: Int, stack: ListBuffer[Double]): Double = {
      if (!expr.isDefinedAt(d)) {
        val value = stack.remove(0)
        println(s"Expression: $expr evaluated to: $value")
        return value
      }

      val ch = expr(d)

      val skip = ch match {
        case _ if Operator.isOp(ch) =>
          if (isUnaryMinus(d)) {
            val (n, i) = parseInt(d + 1)
            -n +=: stack
            i + 1
          } else {
            val n = Operator(ch).evaluate(stack.remove(0), stack.remove(0))
            n +=: stack
            1
          }
        case _ if ch.isDigit =>
          val (n, i) = parseInt(d)
          n +=: stack
          i
        case _ => 1
      }

      evaluate(d + skip, stack)
    }

    evaluate(0, mutable.ListBuffer[Double]())
  }

  sealed trait Operator {
    def evaluate(a: Double, b: Double): Double
  }

  case object Plus extends Operator {
    override def evaluate(a: Double, b: Double): Double = a + b
  }

  case object Minus extends Operator {
    override def evaluate(a: Double, b: Double): Double = b - a
  }

  case object Multiply extends Operator {
    override def evaluate(a: Double, b: Double): Double = a * b
  }

  case object Divide extends Operator {
    override def evaluate(a: Double, b: Double): Double = b / a
  }

  case object Other extends Operator {
    override def evaluate(a: Double, b: Double): Double = ???
  }

  object Operator {
    def isOp(ch: Char): Boolean = {
      Operator(ch) != Other
    }

    def isMinus(ch: Char): Boolean = {
      Operator(ch) == Minus
    }

    def apply(op: Char): Operator = op match {
      case '+' => Plus
      case '-' => Minus
      case 'x' => Multiply
      case '/' => Divide
      case _ => Other
    }
  }

}
