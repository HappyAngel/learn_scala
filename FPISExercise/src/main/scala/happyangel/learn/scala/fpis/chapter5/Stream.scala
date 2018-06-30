package happyangel.learn.scala.fpis.chapter5

import scala.annotation.tailrec

/**
  * Created by LeiXi on 2018/6/3.
  */

sealed trait Stream[+A] {
  import Stream._

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
          case Cons(h, t) => func(n - 1, t(), Stream.cons(h(), newStream))
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
    case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
    case _ => Empty
  }

  def takeWhileByFoldright(p: A => Boolean): Stream[A] = {
      foldRight(Stream[A]())((a,b) =>
          if (p(a))
              Stream.cons(a, b)
          else
              Empty
      )
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = {
      this match {
          case Cons(h, t) => f(h(), t().foldRight(z)(f))
          case _ => z
      }
  }

  def exists(p: A => Boolean): Boolean = {
      foldRight(false)((a,b) => p(a) || b)
  }

  def forAll(p: A => Boolean): Boolean = {
      foldRight(true)((a,b) => p(a) && b)
  }

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = {
      foldRight(Stream.empty[A])((a,b) => if (p(a)) Stream.cons(a, b) else Stream.empty)
  }

  def headOptionViaFoldRight: Option[A] = {
      foldRight(None: Option[A])((a,_) => Some(a))
  }

  def mapViaFoldRight[B](f: A => B): Stream[B] = {
      foldRight(Stream.empty[B])((a,b) => Stream.cons(f(a), b))
  }

  def filterViaFoldRight(f: A => Boolean): Stream[A] = {
      foldRight(Stream.empty[A])((a,b) => if (f(a)) Stream.cons(a,b) else b)
  }

  def append[B>:A](x: => Stream[B]): Stream[B] = {
      foldRight(x)((a,b) => Stream.cons(a,b))
  }

  def flatMapViaFoldRight[B](f: A => Stream[B]): Stream[B] = {
      foldRight(Stream.empty[B])((a,b) => f(a).append(b))
  }

  def mapViaUnfold[B](f: A => B): Stream[B] = {
      unfold(this){
        case Cons(h, t) => Some((f(h()), t()))
        case _ => None
    }
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

  def constant[A](a: A): Stream[A] = {
      Stream.cons(a, constant(a))
  }

  def from(n: Int): Stream[Int] = {
      Stream.cons(n, from(n+1))
  }

  def fibs: Stream[Int] = {
      def fibsHelper(a: Int, b: Int): Stream[Int] = {
          cons(a, fibsHelper(b, a+b))
      }

      fibsHelper(0,1)
  }

  /*
  Corecursive function:
  given an initial state and a function to calculate the next State and Value
   */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
      f(z) match {
        case None => Stream.empty
        case Some((a, s)) =>
          cons(a, unfold(s)(f))
      }
  }

  def onesViaUnfold: Stream[Int] = {
      unfold[Int, String](""){
        _ => Some((1,""))
      }
  }

  def constantViaUnfold(a: Int): Stream[Int] = {
      unfold(1)(_ => Some((a,1)))
  }

  def fromViaUnfold(n: Int): Stream[Int] = {
      unfold(n)(s => Some((s, s+1)))
  }

  def fibsViaUnfold: Stream[Int] = {
      unfold((0,1)){ case(f0, f1) => Some((f0, (f1, f0+f1)))}
  }
}
