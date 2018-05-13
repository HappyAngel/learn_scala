package fpis

/**
  * Created by happyangel on 2018/5/13.
  */

sealed trait Option[+A]
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Chapter4 {

}
