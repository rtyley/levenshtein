package com.madgag.levenshtein

import org.scalatest.{FlatSpec, Matchers}

class LevenshteinTest extends FlatSpec with Matchers {


  implicit val cost = Cost(_ => -2,_ => -2, (x,y) => if (x==y) 2 else -1)

  it should "NWScore" in {
    // Examples taken from https://en.wikipedia.org/wiki/Hirschberg%27s_algorithm#Example
    NeedlemanWunsch.Grid("AGTA","TATGC").scoreLastLine() should equal(Array(-8, -4,  0, -2, -1,  -3))
    NeedlemanWunsch.Grid("CGCA".reverse,"TATGC".reverse).scoreLastLine() should equal(Array(-8, -4, 0, 1, -1, -3))
  }

  it should "Find best alignment" in {
    // Examples taken from https://en.wikipedia.org/wiki/Hirschberg%27s_algorithm#Example
    val grid = NeedlemanWunsch.Grid("AGTACGCA", "TATGC")
    grid.scoreLastLine()
    grid.bestAlignments.map(Edit.asTwoStrings) should contain(Seq("AGTACGCA","--TATGC-"))
  }

//  it should "Hirschberg" in {
//    // Examples taken from https://en.wikipedia.org/wiki/Hirschberg%27s_algorithm#Example
//    Levenshtein.Hirschberg("AGTACGCA","TATGC") should equal(("AGTACGCA", "--TATGC-"))
//  }
}
