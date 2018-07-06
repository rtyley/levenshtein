package com.madgag.levenshtein

object HalfPriceVowels {
  val isVowel: Set[Char] = "aeiouAEIOU".toSet

  def scoreChar(isHalfPrice: Boolean): Int = if (isHalfPrice) -1 else -2

  implicit val cost = Cost(
    a => scoreChar(isVowel(a)),
    a => scoreChar(isVowel(a)),
    (a,b) => if (a==b) 0 else scoreChar(isVowel(a) || isVowel(b))
  )

  def score(X: String, Y: String): Double = Levenshtein.nwScoreLastLine(X, Y).last * -0.5
}
