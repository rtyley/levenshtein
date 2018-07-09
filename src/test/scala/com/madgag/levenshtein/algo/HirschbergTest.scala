package com.madgag.levenshtein.algo

import com.madgag.levenshtein._
import org.scalatest.{FlatSpec, Inspectors, Matchers}

import scala.util.Random

object HirschbergTest {

  implicit val costInWikipediaExample: Cost[Char] = {
    case Delete(_) | Insert(_) => -2
    case s: Substitute[_] => if (s.isAltering) -1 else 2
  }

}

class HirschbergTest extends FlatSpec with Matchers with Inspectors {

  import HirschbergTest.costInWikipediaExample

  it should "get the behaviour it needs when delegating to Needleman-Wunsch" in {
    // Examples taken from https://en.wikipedia.org/wiki/Hirschberg%27s_algorithm#Example
    NeedlemanWunsch.Grid("AGTA", "TATGC").scoreLastLine() should equal(Array(-8, -4, 0, -2, -1, -3))
    NeedlemanWunsch.Grid("CGCA".reverse, "TATGC".reverse).scoreLastLine() should equal(Array(-8, -4, 0, 1, -1, -3))
  }

  it should "match the result given in the Wikipedia article" in {
    // Examples taken from https://en.wikipedia.org/wiki/Hirschberg%27s_algorithm#Example
    Hirschberg.align("AGTACGCA","TATGC").asStringTuple should equal(("AGTACGCA", "--TATGC-"))
  }

  it should "be fine with aligning two empty strings" in {
    Hirschberg.align("", "") shouldBe empty
  }

  it should "align this generated example that failed" in {
    compareHirshWithNW("abcd", "zb")
  }

  private def compareHirshWithNW(a: String, b: String) = {
    val grid = NeedlemanWunsch.Grid(a, b)
    val nwScore = grid.scoreLastLine().last
    val hirschbergAlignment = Hirschberg.align(a, b)

    if (hirschbergAlignment.cost != nwScore) {
      println("\nNeedlemanWunsch")
      grid.align.diagram(_.toString)

      println("\nHirschberg")
      hirschbergAlignment.diagram(_.toString)
    }
    hirschbergAlignment.cost shouldBe nwScore
  }
}