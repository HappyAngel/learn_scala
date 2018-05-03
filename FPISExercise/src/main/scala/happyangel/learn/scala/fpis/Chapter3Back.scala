package happyangel.learn.scala.fpis

/**
  * Created by xionglei on 18-4-26.
  */
object Chapter3Back extends App {

    def init[A](l: List[A]): List[A] = {
        l match {
            case Nil => Nil
            case x :: Nil => Nil
            case x :: ll => x :: init(ll)
        }
    }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
        as match {
            case Nil => z
            case x :: xs => f(x, foldRight(xs, z)(f))
        }
    }

    // 3.10
    @annotation.tailrec
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
       as match {
          case Nil => z
          case x :: xs => foldLeft(xs, f(z, x))(f)
       }
    }

    // 3.9
    def length[A](as: List[A]): Int = {
        foldRight(as, 0)((_,y) => y+1)
    }

    // 3.11
    def sum(as: List[Int]): Int = {
        foldLeft(as, 0)(_+_)
    }

    def product(as: List[Int]): Long = {
        foldLeft(as, 0)(_*_)
    }

    def length2[A](as: List[A]): Int = {
        foldLeft(as, 0)((y,_) => y+1)
    }

    // 3.12
    def reverse[A](as: List[A]): List[A] = {
       foldLeft(as, Nil: List[A])((ll, x) => x::ll)
    }

    println(reverse(List(1,2,3,4,5,6,7,8)))
}
