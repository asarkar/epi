package org.asarkar.epi

import scala.annotation.tailrec

package object strings {
  /*
   * 6.2 Base conversion
   */
  def baseConversion(s: String, b1: Int, b2: Int): String = {
    val num = s.zipWithIndex
      .foldRight(0) { case ((ch, i), n) =>
        n + ch.asDigit * math.pow(b1, s.length - 1 - i).toInt
      }

    Iterator.iterate((num, num)) { case (n, _) =>
      if (n == 0) {
        (-1, -1)
      } else {
        (n / b2, n % b2)
      }
    }
      .drop(1)
      .takeWhile(_._1 >= 0)
      .map(_._2)
      .foldRight(StringBuilder.newBuilder) { case (d, b) =>
        if (d >= 10) {
          b.append((d + '7').toChar)
        } else {
          b.append(d)
        }
      }
      .toString()
  }

  /*
   * 6.7 Compute all mnemonics for a phone number
   */
  def mnemonics(phoneNum: String, dict: IndexedSeq[String]): Iterable[String] = {
    def mnemonics(d: Int, prefix: String): Seq[String] = {
      if (d >= phoneNum.length) {
        Seq(prefix)
      } else {
        for {
          ch <- dict(phoneNum(d).asDigit)
          num <- mnemonics(d + 1, s"$prefix$ch")
        } yield num
      }
    }

    mnemonics(0, "")
  }

  /*
   * 6.9 Convert from Roman to Decimal
   */
  def romanToDecimal(roman: String, charMap: Map[Char, Int]): Int = {
    roman
      .zipWithIndex
      .map { case (ch, i) =>
        val d1 = charMap(ch)
        /*
         * the characters are usually in decreasing order; when not, it's to be subtracted, not added
         */
        d1 * (if (i < roman.length - 1 && d1 < charMap(roman(i + 1))) {
          -1
        } else {
          1
        })
      }
      .sum
  }

  /*
   * 6.12 Implement Run-length encoding
   */
  def rle(s: String): String = {
    @tailrec
    def rle(d: Int, b: StringBuilder): String = {
      if (d >= s.length) return b.toString()

      val ch = s(d)
      val i = Iterator.from(d)
        .dropWhile(x => s.isDefinedAt(x) && s(x) == ch)
        .take(1)
        .reduce(snd[Int])
      rle(i, b.append(i - d).append(ch))
    }

    rle(0, StringBuilder.newBuilder)
  }
}
