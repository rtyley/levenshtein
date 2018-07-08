package com.madgag.levenshtein

import com.madgag.levenshtein.Edit.Typ
import com.madgag.levenshtein.Edit.Typ._


sealed trait Edit {
  def typ: Typ

  def srcOpt: Option[Char]
  def dstOpt: Option[Char]
}

case class Delete(a: Char) extends Edit {
  def typ = Del
  def srcOpt = Some(a)
  def dstOpt = None
}
case class Insert(b: Char) extends Edit {
  def typ = Ins
  def srcOpt = None
  def dstOpt = Some(b)
}
case class Substitute(a: Char, b: Char) extends Edit {
  val typ = Sub
  val srcOpt = Some(a)
  val dstOpt = Some(b)
  val isAltering = a != b
}

object Edit {

  implicit class RichSeqEdits(edits: Seq[Edit]) {
    def cost(implicit cost: Cost): Int = edits.map(cost.cost).sum

    def asStringTuple: (String, String) =
      (edits.map(_.srcOpt.getOrElse('-')).mkString,edits.map(_.dstOpt.getOrElse('-')).mkString)

    def asTwoStrings: Seq[String] =
      Seq(edits.map(_.srcOpt.getOrElse('-')).mkString,edits.map(_.dstOpt.getOrElse('-')).mkString)


    def diagram(costStyler: Int => String)(implicit cost: Cost): Unit = {
      val costs = edits.map(cost.cost)
      val styledCosts = costs.map(costStyler)
      val requiredColWidth = styledCosts.map(_.length).max

      def pad(s: Seq[String]): String = s.map(_.padTo(requiredColWidth,' ')).mkString

      println(pad(edits.map(_.srcOpt.getOrElse('-').toString)))
      val operationIndicators = edits.map { case s: Substitute if s.isAltering => "↓" case _ => "" }
      if (operationIndicators.exists(_.nonEmpty)) {
        println(pad(operationIndicators ))
      }
      println(pad(edits.map(_.dstOpt.getOrElse('-').toString)))


      println(s"${pad(styledCosts)} => Total cost: ${costStyler(costs.sum)}")
    }
  }

  sealed trait Typ

  object Typ {
    object Ins extends Typ
    object Del extends Typ
    object Sub extends Typ
  }

  def insert(s: String): Seq[Edit] = s.map(Insert)
  def delete(s: String): Seq[Edit] = s.map(Delete)

}
