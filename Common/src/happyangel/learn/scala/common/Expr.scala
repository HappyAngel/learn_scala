package happyangel.learn.scala.common

/**
  * Created by xionglei on 17-6-23.
  *
  * from The scala programming language ch15
  *
  */
abstract class Expr

case class Var(name: String) extends Expr
case class Number(num: Double) extends Expr
case class UnOp(operator: String, arg: Expr) extends Expr
case class BinOp(operator: String, left: Expr, right: Expr) extends Expr


