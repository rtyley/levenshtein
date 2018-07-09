package com.madgag.levenshtein.madness

import com.madgag.levenshtein.Edit._
import com.madgag.levenshtein._
import com.madgag.levenshtein.algo.Hirschberg

object HalfPriceVowels {
  val isVowel: Set[Char] = "aeiouAEIOU".toSet

  // because I'm so suspicious of floats, we use Ints for calculation, then convert back for display
  private def scoreChar(isHalfPrice: Boolean): Int = if (isHalfPrice) -1 else -2

  def styleCost(cost: Int): String = -cost match {
    case 0 => ""
    case 1 => "½"
    case 2 => "1"
    case c => f"${c / 2.0f}%.1f"
  }

  implicit val cost: Cost[Char] = (_: Edit[Char]) match {
    case Delete(a) => scoreChar(isVowel(a))
    case Insert(b) => scoreChar(isVowel(b))
    case s: Substitute[Char]  => if (s.isAltering) scoreChar(isVowel(s.a) || isVowel(s.b)) else 0
  }

  def score(X: String, Y: String): Double = {
    val alignment = Hirschberg.align(X, Y)
    alignment.diagram(styleCost)
    alignment.cost * -0.5
  }


}
