package com.madgag.levenshtein


object Cost {
  val normal = Cost(_ => -1,_ => -1, (a,b) => if (a==b) 0 else 1)
}

case class Cost(del: Char=>Int,ins: Char=>Int,sub: (Char,Char)=>Int) {
  def cost(edit: Edit):Int = edit match {
    case Delete(a) => del(a)
    case Insert(b) => ins(b)
    case Substitute(a, b) => sub(a, b)
  }
}
