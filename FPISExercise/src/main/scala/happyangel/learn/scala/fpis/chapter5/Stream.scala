package happyangel.learn.scala.fpis.chapter5

import scala.annotation.tailrec

/**
  * Created by LeiXi on 2018/6/3.
  */

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def toListTailRecur: List[A] = {
    @tailrec
    def func(s: Stream[A], l: List[A]): List[A] = s match {
      case Cons(h, t) => func(t(), (h() :: l))
      case _ => l
    }

    func(this, Nil).reverse
  }

  def take(n: Int): Stream[A] = {
    def func(n: Int, old: Stream[A], newStream: Stream[A]): Stream[A] = {
      if (n <= 0)
        newStream
      else {
        old match {
          case Cons(h, t) => func(n - 1, t(), Cons(h, () => newStream))
          case _ => newStream
        }
      }
    }

    func(n, this, Empty)
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n-1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
    case _ => Empty
  }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    // cache values to avoid repeated evaluation
    lazy val head = hd
    lazy val tail = tl

    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) {
      empty
    } else {
      cons(as.head, apply(as.tail:_*))
    }
  }
}
