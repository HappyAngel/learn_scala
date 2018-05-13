package happyangel.learn.scala.fpis

/**
  * Created by xionglei on 18-4-26.
  */


sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

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

    // 3.13
    def foldLeftViaFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
        foldRight(reverse(as), z)((a,b) => f(b,a))
    }

    // ??
    def foldRightViaFoldLeft[A,B](as: List[A], z: B)(f: (A,B) => B): B = {
        foldLeft(as, (b:B) => b)((g,a) => b => g(f(a,b)))(z)
    }

    // 3.14
    def append[A](as: List[A], z: A): List[A] = {
        foldRight(as, z::Nil)((a, b) => a::b)
    }

    // 3.15
    def concate[A](before: List[A], after: List[A]): List[A] = {
        foldRight(before, after)((a,b) => a::b)
    }

    def concateList[A](ll: List[List[A]]): List[A] = {
        foldLeft(ll, Nil: List[A])(concate)
    }

    // 3.16
    def add1(as: List[Int]): List[Int] = {
       foldRight(as, Nil: List[Int])((a, b) => (a+1 :: b))
    }

    // 3.17
    def doubleToStr(as: List[Double]): List[String] = {
        foldRight(as, Nil: List[String])((a,b) => (a.toString :: b))
    }

    // 3.18
    def map1[A, B](as: List[A])(f: A => B): List[B] = {
        foldRight(as, Nil: List[B])((a,b) => (f(a)::b))
    }

    // use mutation method, it's not visible outside
    def map_2[A,B](l: List[A])(f: A => B): List[B] = {
        val buf = new collection.mutable.ListBuffer[B]
        def go(l: List[A]): Unit = l match {
            case Nil => ()
            case h::t => buf += f(h); go(t)
        }
        go(l)
        List(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
    }

    // 3.19
    def filter[A](as: List[A])(f: A => Boolean): List[A] = {
        foldRight(as, Nil:List[A]){ (a,b) =>
            if (f(a)) a::b else b
        }
    }

    // 3.20
    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
        foldRight(as, Nil: List[B])((a,b) => concate(f(a),b))
    }

    // 3.21
    def filterViaFlatmap[A](as: List[A])(f: A => Boolean): List[A] = {
        flatMap(as)(a => if (f(a)) a::Nil else Nil)
    }

    // 3.22
    def addCorrespondList(as: List[Int], bs: List[Int]): List[Int] = {
        (as, bs) match {
            case (_, Nil) => Nil
            case (Nil, _) => Nil
            case (a::aa, b::bb) => (a+b) :: addCorrespondList(aa, bb)
        }
    }

    def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = {
        (as, bs) match {
            case (_, Nil) => Nil
            case (Nil, _) => Nil
            case (a::aa, b::bb) => f(a,b) :: zipWith(aa,bb)(f)
        }
    }

    // 3.25
    def countNode[A](tree: Tree[A]): Int = {
        tree match {
               case _: Leaf[A] =>
                   1
               case b: Branch[A] =>
                   countNode(b.left) + countNode(b.right) + 1
           }
    }

    // 3.26
    def maxNodeValue(tree: Tree[Int]): Int = {
        tree match {
            case l: Leaf[Int] =>
                l.value
            case b: Branch[Int] =>
                maxNodeValue(b.left) max maxNodeValue(b.right)
        }
    }

    // 3.27
    def depth[A](tree: Tree[A]): Int = {
        tree match {
            case _: Leaf[A] =>
                1
            case b: Branch[A] =>
                (depth(b.left) max depth(b.right)) + 1
        }
    }

    // 3.28
    def mapTree[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
       tree match {
           case Leaf(a) =>
               Leaf(f(a))
           case b: Branch[A] =>
               Branch(mapTree(b.left)(f), mapTree(b.right)(f))
       }
    }

    // 3.29
    def foldTree[A, B](tree: Tree[A])(f: A => B)(g: (B,B) => B): B = {
        tree match {
            case Leaf(a) =>
                f(a)
            case Branch(l, r) =>
                g(foldTree(l)(f)(g), foldTree(r)(f)(g))
        }
    }

    // 3.29-1
    def sizeViaFold[A](t: Tree[A]): Int = {
        foldTree(t)(_ => 1)(1 + _ + _)
    }

    def maximumViaFold[A](t: Tree[Int]): Int = {
        foldTree(t)(a=>a)(_ max _)
    }

    def depthViaFold[A](t: Tree[A]): Int = {
        foldTree(t)(_=>1)(1 + _ max _)
    }

    def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] = {
        foldTree(t)(a=> Leaf(f(a)):Tree[B])((a,b) => Branch(a,b))
    }

    println(sizeViaFold(Branch(Branch(Branch(Leaf(1), Leaf(2)), Leaf(2)), Branch(Leaf(3), Leaf(4)))))
}
