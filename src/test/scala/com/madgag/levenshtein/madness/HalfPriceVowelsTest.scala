package com.madgag.levenshtein.madness

import org.scalactic.TolerantNumerics
import org.scalatest.FlatSpec

class HalfPriceVowelsTest extends FlatSpec {

  implicit val doubleEq = TolerantNumerics.tolerantDoubleEquality(1e-4f)

  it should "respect Pascal's examples" in {
    assert(HalfPriceVowels.score("abba", "aa") === 2.0)

    assert(HalfPriceVowels.score("Luc", "Luke") === 1.5)

    assert(HalfPriceVowels.score("Pascal", "Pascale") === 0.5)

    assert(HalfPriceVowels.score(
      "Pneumonoultramicroscopicsilicovolcanoconiosis",
      "Pseudopseudohypoparathyroidism"
    ) === 26.0) // Not 26.5 or 33, so far as I can see...
  }
}
