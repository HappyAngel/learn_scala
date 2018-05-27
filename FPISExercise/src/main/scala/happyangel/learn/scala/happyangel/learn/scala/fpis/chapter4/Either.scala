package happyangel.learn.scala.fpis.chapter4

/**
  * Created by LeiXi on 2018/5/27.
  */

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E,B] = {
    this match {
      case Left(l) => Left(l)
      case Right(r) => Right(f(r))
    }
  }
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE,B] = {
    this match {
      case Left(l) => Left(l)
      case Right(r) => f(r)
    }
  }
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE,B] = {
    this match {
      case Left(_) => b
      case Right(r) => Right(r)
    }
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A,B) => C): Either[EE,C] = {
    for {
      aa <- this;
      bb <- b
    } yield {
      f(aa, bb)
    }
  }
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

case class Person(name: Name, age: Age)
sealed class Name(val value: String)
sealed class Age(val value: Int)

object Either extends App {
  def sequence[E,A](es: List[Either[E, A]]): Either[E, List[A]] = {
    es match {
      case Nil => Right(Nil)
      case x::xx => x flatMap { v =>
        sequence(xx) map { vv =>
          v :: vv
        }
      }
    }
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    as match {
      case Nil => Right(Nil)
      case x::xx =>
        f(x) flatMap { v=>
          traverse(xx)(f) map { vv =>
            v :: vv
          }
        }
    }
  }

  def mkName(name: String): Either[String, Name] = {
    if (name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))
  }

  def mkAge(age: Int): Either[String, Age] = {
    if (age < 0)
      Left("Age is out of range.")
    else
      Right(new Age(age))
  }

  def mkPerson(name: String, age: Int): Either[String, Person] = {
    mkName(name).map2(mkAge(age))(Person(_, _))
  }
}
