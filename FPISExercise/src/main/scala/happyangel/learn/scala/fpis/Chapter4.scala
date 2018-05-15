package fpis

/**
  * Created by happyangel on 2018/5/13.
  */

sealed trait MyOption[+A] {
  def map[B](f: A => B): MyOption[B] = {
    this match {
      case MySome(v) =>
        MySome(f(v))
      case MyNone =>
        MyNone
    }
  }
  def flatMap[B](f: A => MyOption[B]): MyOption[B] = {
    this match {
      case MySome(v) =>
        f(v)
      case MyNone =>
        MyNone
    }
  }
  def getOrElse[B >: A](default: => B): B = {
    this match {
      case MySome(v) =>
        v
      case MyNone =>
        default
    }
  }

  def orElse[B >: A](ob: => MyOption[B]): MyOption[B] = {
    this match {
      case MySome(v) =>
        this
      case MyNone =>
        ob
    }
  }
  def filter(f: A => Boolean): MyOption[A] = {
    this match {
      case MySome(v) =>
        if (f(v))
          this
        else
          MyNone
      case MyNone =>
        MyNone
    }
  }
}
case class MySome[+A](get: A) extends MyOption[A]
case object MyNone extends MyOption[Nothing]

object Chapter4 {

}
