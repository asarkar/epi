package org.asarkar.epi

import scala.collection.mutable.ListBuffer

package object stacks {
  /*
   * 8.3 Test a string for well-formedness
   */
  def isWellFormed(s: String): Boolean = {
    if (s.length % 2 != 0) false
    else {
      val opening = Map(
        '(' -> ')',
        '{' -> '}',
        '[' -> ']',
      )
      val closing = opening
        .map(_.swap)

      val expr = ListBuffer.empty[Char]

      s.foldLeft(true) { (valid, c) =>
        if (closing.contains(c)) {
          valid && expr.nonEmpty && opening(expr.remove(0)) == c
        } else if (valid) {
          expr.prepend(c)
          valid
        } else valid
      } && expr.isEmpty
    }
  }
}
