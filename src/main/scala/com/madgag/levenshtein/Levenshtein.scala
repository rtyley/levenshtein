package com.madgag.levenshtein

import com.jakewharton.fliptables.FlipTable
import com.madgag.levenshtein.Levenshtein.EditType
import com.madgag.levenshtein.Levenshtein.EditType.{Del, Ins, Sub}


object Cost {
  val normal = Cost(_ => -1,_ => -1, (a,b) => if (a==b) 0 else 1)
}

object Edit {
  def asTwoStrings(edits: Seq[Edit]): Seq[String] =
    Seq(edits.map(_.srcOpt.getOrElse('-')).mkString,edits.map(_.dstOpt.getOrElse('-')).mkString)

  def printWithCosts(edits: Seq[Edit])(implicit cost: Cost): Unit = {
    val width = 3
    def pad(s: Seq[String]): String = s.map(_.padTo(width,' ')).mkString

    println(pad(edits.map(_.srcOpt.getOrElse('-').toString)))
    println(pad(edits.map(_.dstOpt.getOrElse('-').toString)))
    val costs = edits.map(cost.cost)
    val costsAndTotal = costs :+ costs.sum
    println(pad(costsAndTotal.map(_.toString)))
  }
}

sealed trait Edit {
  val typ: EditType

  val srcOpt: Option[Char]
  val dstOpt: Option[Char]
}

case class Delete(a: Char) extends Edit {
  val typ = Del
  val srcOpt = Some(a)
  val dstOpt = None
}
case class Insert(b: Char) extends Edit {
  val typ = Ins
  val srcOpt = None
  val dstOpt = Some(b)
}
case class Substitute(a: Char, b: Char) extends Edit {
  val typ = Sub
  val srcOpt = Some(a)
  val dstOpt = Some(b)
  val isAltering = a != b
}

case class Alignment(x: String, y: String)

case class Cost(del: Char=>Int,ins: Char=>Int,sub: (Char,Char)=>Int) {
  def cost(edit: Edit):Int = edit match {
    case Delete(a) => del(a)
    case Insert(b) => ins(b)
    case Substitute(a, b) => sub(a, b)
  }
}

object NeedlemanWunsch {

  object AvailableEdits {
    val JustIns = Set[EditType](Ins)
    val JustDel = Set[EditType](Del)
    val All = Set(Ins, Del, Sub)
  }

  case class Coord(x: Int, y: Int) {

    lazy val availableEdits: Set[EditType] =
      if (x>0 && y>0) AvailableEdits.All
      else if (x>0) AvailableEdits.JustDel
      else if (y>0) AvailableEdits.JustIns
      else Set.empty

    def on(typ: EditType): Coord = typ match {
      case Del => copy(x = x - 1)
      case Ins => copy(y = y - 1)
      case Sub => copy(x = x - 1, y = y - 1)
    }
  }

  case class Grid(X: String, Y: String)(implicit cost: Cost) {

    private val coordOfOptimalResult = Coord(X.length,Y.length)

    private val d = Array.ofDim[Int](X.length+1, Y.length+1)

    def previouslyCalculatedScore(c: Coord): Int = d(c.x)(c.y)

    def X_(c: Coord):Char = X(c.x)
    def Y_(c: Coord):Char = Y(c.y)

    def editsAt(c: Coord): Set[Edit] = c.availableEdits.map(editAt(c,_))

    def editAt(c: Coord, typ: EditType): Edit = {
      val prior = c on typ
      typ match {
        case Ins => Insert(Y_(prior))
        case Del => Delete(X_(prior))
        case Sub => Substitute(X_(prior), Y_(prior))
      }
    }

    def scoreFromPreviouslyCalculatedScores(c: Coord, edit: Edit): Int =
      previouslyCalculatedScore(c on edit.typ) + cost.cost(edit)

    def calculateScore(c: Coord): Int = {
      val availableEdits = editsAt(c)
      if (availableEdits.isEmpty) 0 else availableEdits.map(scoreFromPreviouslyCalculatedScores(c, _)).max
    }

    def bestEditsAt(c: Coord): Set[Edit] = {
      val bestScore = previouslyCalculatedScore(c)
      for {
        edit <- editsAt(c)
        score = scoreFromPreviouslyCalculatedScores(c, edit) if score == bestScore
      } yield edit
    }

    def bestAlignments: Stream[Seq[Edit]] = {

      def chase(c: Coord): Stream[Seq[Edit]] = {
        // println(s"Chasing $c")
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

object Levenshtein {

  sealed trait EditType

  object EditType {
    object Ins extends EditType
    object Del extends EditType
    object Sub extends EditType
  }



  def Hirschberg(X: String,Y: String)(implicit cost: Cost): (String,String) = (X.length,Y.length) match {
    case (0,_) => ("-" * Y.length, Y)
    case (_,0) => (X, "-" * X.length)
//    case (1,_) | (_,1) => ??? // NeedlemanWunsch(X,Y)
    case _ =>
      val (xL,xR) = X.splitAt(X.length / 2)

      val scoreL = NeedlemanWunsch.Grid(xL, Y).scoreLastLine()
      val scoreR = NeedlemanWunsch.Grid(xR.reverse, Y.reverse).scoreLastLine().reverse

      val optimalYsplitIndex = (0 until Y.length).maxBy(i => scoreL(i) + scoreR(i))

      val (yL,yR) = Y.splitAt(optimalYsplitIndex)

      val ((zL,wL), (zR,wR)) = (Hirschberg(xL, yL), Hirschberg(xR, yR))
      (zL+zR, wL+wR)
  }
}
