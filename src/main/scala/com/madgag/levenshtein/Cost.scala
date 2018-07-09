package com.madgag.levenshtein

trait Cost[T] {
  def cost(edit: Edit[T]): Int
}
