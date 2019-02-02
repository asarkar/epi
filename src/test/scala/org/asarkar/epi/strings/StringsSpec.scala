package org.asarkar.epi.strings

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor2}

class StringsSpec extends FlatSpec with TableDrivenPropertyChecks {
  "strings" should "convert between bases" in {
    baseConversion("615", 7, 13) shouldBe "1A7"
  }

  it should "compute all mnemonics for a phone number" in {
    val dict = Array("", "", "abc", "def", "ghi", "jkl",
      "mno", "pqrs", "tuv", "wxyz")
    mnemonics("234", dict) should contain only(
      "adg", "adh", "adi", "aeg", "aeh", "aei", "afg", "afh", "afi", "bdg",
      "bdh", "bdi", "beg", "beh", "bei", "bfg", "bfh", "bfi", "cdg", "cdh",
      "cdi", "ceg", "ceh", "cei", "cfg", "cfh", "cfi"
    )
  }

  it should "convert from Roman to Decimal" in {
    val charMap = Map(
      'I' -> 1,
      'V' -> 5,
      'X' -> 10,
      'L' -> 50,
      'C' -> 100,
      'D' -> 500,
      'M' -> 1000
    )
    val numbers: TableFor2[String, Int] =
      Table(
        ("roman", "decimal"),
        ("XXXXXIIIIIIIII", 59),
        ("LVIIII", 59),
        ("LIX", 59)
      )

    forAll(numbers) { (roman: String, decimal: Int) =>
      romanToDecimal(roman, charMap) shouldBe decimal
    }
  }

  it should "implement Run-length encoding" in {
    rle("aaabbcdeee") shouldBe "3a2b1c1d3e"
  }
}
