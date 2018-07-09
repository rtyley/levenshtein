package com.madgag.levenshtein.algo

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class NeedlemanWunschTest extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  import HirschbergTest.costInWikipediaExample

  it should "Handle empty strings" in {
    NeedlemanWunsch.Grid("", "").align shouldBe empty
  }

  it should "Find best alignment" in {
    NeedlemanWunsch.Grid("AGTACGCA", "TATGC").bestAlignments.map(_.asTwoStrings) should contain(Seq("AGTACGCA", "--TATGC-"))
  }

  it should "always find alignments that have the best score" in {
    implicit val generatorDrivenConfig = PropertyCheckConfig(minSize = 0, maxSize = 20, minSuccessful = 1000)

    forAll (Gen.alphaStr, Gen.alphaStr) { (a: String, b: String) =>
      val grid = NeedlemanWunsch.Grid(a, b)
      val nwScore = grid.scoreLastLine().last

      grid.align.cost shouldBe nwScore
    }
  }

  it should "handle this dumb string" in {
    val grid = NeedlemanWunsch.Grid("eunfbi", "n")
    grid.scoreLastLine().last shouldBe -8
    val alignment = grid.align
    alignment.asStringTuple shouldBe(("eunfbi", "--n---"))
    alignment.cost shouldBe -8
  }

  it should "always score reversed strings with the same score" in {
    implicit val generatorDrivenConfig = PropertyCheckConfig(minSize = 0, maxSize = 20, minSuccessful = 1000)

    forAll (Gen.alphaStr, Gen.alphaStr) { (a: String, b: String) =>
      NeedlemanWunsch.Grid(a.reverse, b.reverse).scoreLastLine().last shouldBe NeedlemanWunsch.Grid(a, b).scoreLastLine().last
    }
  }
}
