package happyangel.learn.scala.fpis.chapter6

/**
  * Created by happyangel on 2018/7/22.
  */

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object CandyMachine extends App {

  import State._

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    State[Machine, (Int, Int)] { s =>
       inputs match {
        case h::t =>
          h match {
            case Coin if s.locked && s.candies > 0 =>
              val m = Machine(false, s.candies, s.coins+1)
              simulateMachine(t).run(m)
            case Turn if !s.locked && s.candies > 0 =>
              val m = Machine(true, s.candies-1, s.coins)
              simulateMachine(t).run(m)
            case _ =>
              simulateMachine(t).run(s)
          }
        case Nil =>
          ((s.candies,s.coins), s)
      }
    }
  }



  print(DateTime)
}
