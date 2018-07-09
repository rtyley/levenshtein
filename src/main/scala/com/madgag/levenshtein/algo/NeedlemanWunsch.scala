package com.madgag.levenshtein.algo

import com.madgag.levenshtein.Edit.Typ
import com.madgag.levenshtein.Edit.Typ.{Del, Ins, Sub}
import com.madgag.levenshtein._

object NeedlemanWunsch {

  object AvailableEdits {
    val JustIns = Set[Typ](Ins)
    val JustDel = Set[Typ](Del)
    val All = Set(Ins, Del, Sub)
  }

  case class Coord(x: Int, y: Int) {

    lazy val availableEdits: Set[Typ] =
      if (x>0 && y>0) AvailableEdits.All
      else if (x>0) AvailableEdits.JustDel
      else if (y>0) AvailableEdits.JustIns
      else Set.empty

    def on(typ: Typ): Coord = typ match {
      case Del => copy(x = x - 1)
      case Ins => copy(y = y - 1)
      case Sub => copy(x = x - 1, y = y - 1)
    }
  }

  case class Grid[T](X: Seq[T], Y: Seq[T])(implicit cost: Cost[T]) {

    private val coordOfOptimalResult = Coord(X.length,Y.length)

    private val d = Array.ofDim[Int](X.length+1, Y.length+1)

    def previouslyCalculatedScore(c: Coord): Int = d(c.x)(c.y)

    def X_(c: Coord):T = X(c.x)
    def Y_(c: Coord):T = Y(c.y)

    def editsAt(c: Coord): Set[Edit[T]] = c.availableEdits.map(editAt(c,_))

    def editAt(c: Coord, typ: Typ): Edit[T] = {
      val prior = c on typ
      typ match {
        case Ins => Insert(Y_(prior))
        case Del => Delete(X_(prior))
        case Sub => Substitute(X_(prior), Y_(prior))
      }
    }

    def scoreFromPreviouslyCalculatedScores(c: Coord, edit: Edit[T]): Int =
      previouslyCalculatedScore(c on edit.typ) + cost.cost(edit)

    def calculateScore(c: Coord): Int = {
      val availableEdits = editsAt(c)
      if (availableEdits.isEmpty) 0 else availableEdits.map(scoreFromPreviouslyCalculatedScores(c, _)).max
    }

    def bestEditsAt(c: Coord): Set[Edit[T]] = {
      val bestScore = previouslyCalculatedScore(c)
      for {
        edit <- editsAt(c)
        score = scoreFromPreviouslyCalculatedScores(c, edit) if score == bestScore
      } yield edit
    }

    def align: Seq[Edit[T]] = bestAlignments.head

    def bestAlignments: Stream[Seq[Edit[T]]] = {
      scoreLastLine()

      def chase(c: Coord): Stream[Seq[Edit[T]]] = {
        val bestEdits = bestEditsAt(c)
        if (bestEdits.isEmpty) Stream(Seq.empty) else bestEdits.toStream.flatMap(edit => chase(c on edit.typ).map(_ :+ edit))
      }

      chase(coordOfOptimalResult)
    }

    def scoreLastLine(): Array[Int] = {
      for {
        i <- 0 to X.length
        j <- 0 to Y.length
      } d(i)(j) = calculateScore(Coord(i, j))

      d(X.length)
    }
  }
}
