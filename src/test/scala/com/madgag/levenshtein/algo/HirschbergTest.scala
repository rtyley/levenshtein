package com.madgag.levenshtein.algo

import com.madgag.levenshtein.{Cost, Edit}
import org.scalatest.{FlatSpec, Matchers}

class HirschbergTest extends FlatSpec with Matchers {

  implicit val costInWikipediaExample = Cost(_ => -2, _ => -2, (x, y) => if (x == y) 2 else -1)

  it should "have the behaviour it needs when delegating to Needleman-Wunsch" in {
    // Examples taken from https://en.wikipedia.org/wiki/Hirschberg%27s_algorithm#Example
    NeedlemanWunsch.Grid("AGTA", "TATGC").scoreLastLine() should equal(Array(-8, -4, 0, -2, -1, -3))
    NeedlemanWunsch.Grid("CGCA".reverse, "TATGC".reverse).scoreLastLine() should equal(Array(-8, -4, 0, 1, -1, -3))
  }

  it should "match the result given in the Wikipedia article" in {
    // Examples taken from https://en.wikipedia.org/wiki/Hirschberg%27s_algorithm#Example
    Edit.asStringTuple(Hirschberg.align("AGTACGCA","TATGC")) should equal(("AGTACGCA", "--TATGC-"))
  }
}