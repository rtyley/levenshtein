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

  sealed trait Typ

  object Typ {
    object Ins extends Typ
    object Del extends Typ
    object Sub extends Typ
  }

  def insert(s: String): Seq[Edit] = s.map(Insert)
  def delete(s: String): Seq[Edit] = s.map(Delete)

  def asStringTuple(edits: Seq[Edit]): (String, String) =
    (edits.map(_.srcOpt.getOrElse('-')).mkString,edits.map(_.dstOpt.getOrElse('-')).mkString)


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
