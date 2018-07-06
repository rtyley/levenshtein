package com.madgag.levenshtein

import org.scalatest.{FlatSpec, Matchers}

class LevenshteinTest extends FlatSpec with Matchers {

  it should "NWScore" in {
    implicit val cost = Cost(_ => -2,_ => -2, (x,y) => if (x==y) 2 else -1)

    // Examples taken from https://en.wikipedia.org/wiki/Hirschberg%27s_algorithm#Example
    Levenshtein.nwScoreLastLine("AGTA","TATGC") should equal(Array(-8, -4,  0, -2, -1,  -3))
    Levenshtein.nwScoreLastLine("CGCA".reverse,"TATGC".reverse) should equal(Array(-8, -4, 0, 1, -1, -3))
  }

}
