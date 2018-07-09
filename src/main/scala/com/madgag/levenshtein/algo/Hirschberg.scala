package com.madgag.levenshtein.algo

import com.madgag.levenshtein.{Cost, Edit}
import com.madgag.levenshtein.Edit.{delete, insert}

object Hirschberg {
  def align[T](X: Seq[T],Y: Seq[T])(implicit cost: Cost[T]): Seq[Edit[T]] = (X.length,Y.length) match {
    case (0,_) => insert(Y)
    case (_,0) => delete(X)
    case (1,_) | (_,1) => NeedlemanWunsch.Grid(X,Y).align
    case _ =>
      val (xL,xR) = X.splitAt(X.length / 2)

      val scoreL = NeedlemanWunsch.Grid(xL, Y).scoreLastLine()
      val scoreR = NeedlemanWunsch.Grid(xR.reverse, Y.reverse).scoreLastLine().reverse

      val optimalYsplitIndex = (0 to Y.length).maxBy(i => scoreL(i) + scoreR(i))

      val (yL,yR) = Y.splitAt(optimalYsplitIndex)

      align(xL, yL) ++ align(xR, yR)
  }
}
