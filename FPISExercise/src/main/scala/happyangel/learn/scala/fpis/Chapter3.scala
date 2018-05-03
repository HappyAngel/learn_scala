package fpis

/**
  * Created by xionglei on 2018/4/23.
  */
object Chapter3 extends App {

  def tail[T](ll: List[T]): List[T] = {
    ll match {
      case Nil => Nil
      case _ :: Nil => Nil
      case _ :: xx => xx
    }
  }

  def setHead[T](ll: List[T], x: T): List[T] = {
    ll match {
      case Nil => Nil
      case _ :: Nil => List(x)
      case _ :: xx => x :: xx
    }
  }

  def drop[T](ll: List[T], n: Int): List[T] = {
    def dropInternal[T](ll: List[T], n: Int): List[T] = {
      if (n <= 0)
        ll
      else
        dropInternal(tail(ll), n-1)
    }

    dropInternal(ll, n)
  }

  def dropWhile[T](ll: List[T], f: T => Boolean): List[T] = {
    def dropInternal[T](ll: List[T], f: T => Boolean): List[T] = {
      ll match {
        case Nil => Nil
        case x :: Nil => if (f(x)) Nil else ll
        case x :: xx => if (f(x)) dropInternal(xx, f) else x :: dropInternal(xx, f)
      }
    }

    dropInternal(ll, f)
  }

  print(dropWhile[Int](List(1,2,3,4,5), _%2==0))
}
