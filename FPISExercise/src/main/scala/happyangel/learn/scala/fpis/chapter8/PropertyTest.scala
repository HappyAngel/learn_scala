package happyangel.learn.scala.fpis.chapter8

import happyangel.learn.scala.fpis.chapter6.{RNG, State, testRNG}
import happyangel.learn.scala.fpis.chapter8.PropertyTest.Prop.{FailedCase, SuccessCount}

/**
  * Created by happyangel on 2018/8/19.
  */

object PropertyTest {

  trait Prop {
    def check: Either[(FailedCase, SuccessCount), SuccessCount]
    def &&(p: Prop): Prop =  ???
  }

  object Prop {
    type FailedCase = String
    type SuccessCount = Int
  }

  case class Gen[A](sample: State[RNG, A])

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(testRNG.nonNegativeInt).map { n =>
      start + n % (stopExclusive - start)
    })
  }

  def unit[A](a: => A): Gen[A] = {
    Gen(State.unit(a))
  }

  def boolean: Gen[Boolean] = {
    Gen(State(testRNG.boolean))
  }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    Gen(State.sequence(List.fill(n)(g.sample)))
  }

  // def listOf[A](a: Gen[A]): Gen[List[A]]

  // def forAll[A](a: Gen[A])(f: A => Boolean): Prop

}
