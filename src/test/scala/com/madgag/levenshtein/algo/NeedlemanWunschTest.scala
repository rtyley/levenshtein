package com.madgag.levenshtein.algo

import com.madgag.levenshtein.{Cost, Edit}
import org.scalatest.{FlatSpec, Matchers}

class NeedlemanWunschTest extends FlatSpec with Matchers {

  implicit val costInWikipediaExample = Cost(_ => -2, _ => -2, (x, y) => if (x == y) 2 else -1)

  it should "Find best alignment" in {
    // Examples taken from https://en.wikipedia.org/wiki/Hirschberg%27s_algorithm#Example
    val grid = NeedlemanWunsch.Grid("AGTACGCA", "TATGC")
    grid.scoreLastLine()
    grid.bestAlignments.map(_.asTwoStrings) should contain(Seq("AGTACGCA", "--TATGC-"))
  }
}
