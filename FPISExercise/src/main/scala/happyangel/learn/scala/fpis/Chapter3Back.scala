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
    def foldRightTailRecur[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
        as match {
            case Nil => z
            case x :: xs => foldRightTailRecur(xs, f(x, z))(f)
        }
    }

    // 3.9
    def length[A](as: List[A]): Int = {
        foldRight(as, 0)((_,y) => y+1)
    }

    println(length(List(1,2,3,4,5,6,7,8)))
}
