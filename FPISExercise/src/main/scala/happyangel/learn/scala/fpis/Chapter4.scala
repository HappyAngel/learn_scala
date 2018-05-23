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

    this flatMap { v =>
      if (f(v))
        MySome(v)
      else
        MyNone
    }
  }
}
case class MySome[+A](get: A) extends MyOption[A]
case object MyNone extends MyOption[Nothing]

object Chapter4 extends App {

  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty)
      None
    else
      Some(xs.sum / xs.size)
  }
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap { mm =>
      mean(xs.map(x => math.pow(x-mm,2)))
    }
  }

  def Try[A](a: => A): Option[A] = {
    try Some(a)
    catch { case _: Exception => None }
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    for {
      aa <- a
      bb <- b
    } yield {
      f(aa,bb)
    }
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    if (a.exists(_.isEmpty)) {
      None
    } else {
      Some(a.map(_.get))
    }
  }

  def sequenceRur[A](a: List[Option[A]]): Option[List[A]] = {
    a match {
      case Nil =>
        Some(Nil)
      case h::t => h flatMap (hh => sequenceRur(t) map (hh::_))
    }
  }

  def sequenceViaFold[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldRight[Option[List[A]]](Some(Nil))((a, b) => map2(a,b)(_::_))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a match {
      case Nil => Some(Nil)
      case h::t => f(h) flatMap (hh => traverse(t)(f) map(hh::_))
    }
  }

  println(traverse[String, Int](List("2", "3", "sdd", "5"))(b=>Try(b.toInt)))
}
