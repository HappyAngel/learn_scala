package happyangel.learn.scala.common

/**
  * Created by happyangel on 2017/7/15.
  *
  *  to demonstrate The Scala programming language ch20
  */
abstract class Currency {
  val amount: Long
  def designation: String

  override def toString: String = amount + " " + designation

  def + (that: Currency): Currency = {
    amount + that.amount
  }
}
