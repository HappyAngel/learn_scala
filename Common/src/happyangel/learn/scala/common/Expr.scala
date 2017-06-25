package happyangel.learn.scala.common

import happyangel.learn.scala.common.Element
/**
  * Created by xionglei on 17-6-23.
  *
  * from The scala programming language ch15
  *
  */
sealed abstract class Expr

case class Var(name: String) extends Expr
case class Number(num: Double) extends Expr
case class UnOp(operator: String, arg: Expr) extends Expr
case class BinOp(operator: String, left: Expr, right: Expr) extends Expr

class ExprFormatter {
    // Contains operators in groups of increasing precedence
    private val opGroups =
        Array(
            Set("|", "||"),
            Set("&", "&&"),
            Set("^"),
            Set("==", "!="),
            Set("<", "<=", ">", ">="),
            Set("+", "-"),
            Set("*", "%")
        )

    private val precedence = {
        val assocs =
            for {
                i <- 0 until opGroups.length
                op <- opGroups(i)
            } yield op -> i
        assocs.toMap
    }

    // unary precedence is larger than binary
    private val unaryPrecedence = opGroups.length
    private val fractionPrecedence = -1

    private def format(e: Expr, enclPrec: Int): Element = e match {

    }
}

