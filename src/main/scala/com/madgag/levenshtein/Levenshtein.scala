package com.madgag.levenshtein

import java.lang.Math.max


object Cost {
  val normal = Cost(_ => -1,_ => -1, (_,_) => -1)
}


case class Cost(del: Char=>Int,ins: Char=>Int,sub: (Char,Char)=>Int)

object Levenshtein {

  def nwScoreLastLine(X: String, Y: String)(implicit cost: Cost): Array[Int] = {
    val score = Array.ofDim[Int](X.length+1, Y.length+1)
    score(0)(0) = 0
    for (j <- 0 until Y.length) {
      score(0)(j+1) = score(0)(j) + cost.ins(Y(j))
    }
    println()
    println(score(0).mkString(" "))

    for (i <- 0 until X.length) {
      score(i+1)(0) = score(i)(0) + cost.del(X(i))
      for (j <- 0 until Y.length) {
        val scoreSub = score(i  )(j  ) + cost.sub(X(i), Y(j))
        val scoreDel = score(i  )(j+1) + cost.del(X(i))
        val scoreIns = score(i+1)(j  ) + cost.ins(Y(j))
        score(i+1)(j+1) = max(scoreSub, max(scoreDel, scoreIns))
      }
      println(score(i+1).mkString(" "))
    }
    score(X.length)
  }


  def Hirschberg(X: String,Y: String)(implicit cost: Cost): (String,String) = (X.length,Y.length) match {
    case (0,_) => ("-" * Y.length, Y)
    case (_,0) => (X, "-" * X.length)
    case (1,_) | (_,1) => ??? // NeedlemanWunsch(X,Y)
    case _ =>
      val (xL,xR) = X.splitAt(X.length / 2)

      val scoreL = nwScoreLastLine(xL, Y)
      val scoreR = nwScoreLastLine(xR.reverse, Y.reverse).reverse

      val optimalYsplitIndex = (0 until Y.length).maxBy(i => scoreL(i) + scoreR(i))

      val (yL,yR) = Y.splitAt(optimalYsplitIndex)

      val ((zL,wL), (zR,wR)) = (Hirschberg(xL, yL), Hirschberg(xR, yR))
      (zL+zR, wL+wR)
  }
}
