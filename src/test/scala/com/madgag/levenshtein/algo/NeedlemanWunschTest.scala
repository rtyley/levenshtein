package com.madgag.levenshtein.algo

import com.madgag.levenshtein.Cost
import org.scalatest.{FlatSpec, Matchers}

class NeedlemanWunschTest extends FlatSpec with Matchers {

  implicit val costInWikipediaExample = Cost(_ => -2, _ => -2, (x, y) => if (x == y) 2 else -1)

  it should "Handle empty strings" in {
    NeedlemanWunsch.Grid("", "").align shouldBe empty
  }

  it should "Find best alignment" in {
    NeedlemanWunsch.Grid("AGTACGCA", "TATGC").bestAlignments.map(_.asTwoStrings) should contain(Seq("AGTACGCA", "--TATGC-"))
  }
}
