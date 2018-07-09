package com.madgag.levenshtein

import com.madgag.levenshtein.Edit.Typ
import com.madgag.levenshtein.Edit.Typ._


sealed trait Edit[T] {
  def typ: Typ

  def srcOpt: Option[T]
  def dstOpt: Option[T]
}

case class Delete[T](a: T) extends Edit[T] {
  def typ = Del
  def srcOpt = Some(a)
  def dstOpt = None
}
case class Insert[T](b: T) extends Edit[T] {
  def typ = Ins
  def srcOpt = None
  def dstOpt = Some(b)
}
case class Substitute[T](a: T, b: T) extends Edit[T] {
  val typ = Sub
  val srcOpt = Some(a)
  val dstOpt = Some(b)
  val isAltering = a != b
}

object Edit {

  implicit class RichSeqEdits[T](edits: Seq[Edit[T]]) {
    def cost(implicit cost: Cost[T]): Int = edits.map(cost.cost).sum

    def asStringTuple: (String, String) =
      (edits.map(_.srcOpt.getOrElse('-')).mkString,edits.map(_.dstOpt.getOrElse('-')).mkString)

    def asTwoStrings: Seq[String] =
      Seq(edits.map(_.srcOpt.getOrElse('-')).mkString,edits.map(_.dstOpt.getOrElse('-')).mkString)


    def diagram(costStyler: Int => String)(implicit cost: Cost[T]): Unit = {
      val costs = edits.map(cost.cost)
      val styledCosts = costs.map(costStyler)

      val requiredColWidth = if (edits.isEmpty) 1 else {
        val minRequired = styledCosts.map(_.length).max
        if (minRequired == 1) 1 else minRequired+1
      }

      def pad(s: Seq[String]): String = s.map(_.padTo(requiredColWidth,' ')).mkString

      println(pad(edits.map(_.srcOpt.getOrElse('-').toString)))
      val operationIndicators = edits.map {
        case s: Substitute[_] => if (s.isAltering) "↓" else "⋮"
        case _ => ""
      }
      if (operationIndicators.exists(_.nonEmpty)) {
        println(pad(operationIndicators ))
      }
      println(pad(edits.map(_.dstOpt.getOrElse('-').toString)))


      println(s"${pad(styledCosts)} => Total cost: ${costStyler(costs.sum)}\n")
    }
  }

  sealed trait Typ

  object Typ {
    object Ins extends Typ
    object Del extends Typ
    object Sub extends Typ
  }

  def insert[T](s: Seq[T]): Seq[Edit[T]] = s.map(Insert[T])
  def delete[T](s: Seq[T]): Seq[Edit[T]] = s.map(Delete[T])

}
