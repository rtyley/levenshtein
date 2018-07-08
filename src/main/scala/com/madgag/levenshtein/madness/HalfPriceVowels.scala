package com.madgag.levenshtein.madness

import com.madgag.levenshtein.algo.NeedlemanWunsch
import com.madgag.levenshtein.{Cost, Edit}

object HalfPriceVowels {
  val isVowel: Set[Char] = "aeiouAEIOU".toSet

  def scoreChar(isHalfPrice: Boolean): Int = if (isHalfPrice) -1 else -2

  implicit val cost = Cost(
    a => scoreChar(isVowel(a)),
    a => scoreChar(isVowel(a)),
    (a,b) => if (a==b) 0 else scoreChar(isVowel(a) || isVowel(b))
  )

  def score(X: String, Y: String): Double = {

    val grid = NeedlemanWunsch.Grid(X, Y)

    val s = grid.scoreLastLine().last * -0.5

    for (alignment <- grid.bestAlignments.take(3)) {
      println()
      Edit.printWithCosts(alignment)
    }

    s
  }
}
