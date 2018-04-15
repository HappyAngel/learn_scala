package happyangel.learn.scala.fpis

import scala.annotation.Annotation

/**
  * Created by xionglei on 18-4-12.
  */
object Chapter2 extends App {
    def fib(n: Int): Int = {
        @annotation.tailrec
        def tmp(prev1: Int, prev0: Int, n: Int): Int = {
            if (n == 3)
                prev0 + prev1
            else
                tmp(prev0, prev0+prev1, n-1)
        }

        require(n > 0)
        if (n == 1)
            0
        else if (n == 2)
            1
        else
            tmp(0, 1, n)
    }

    def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
        @annotation.tailrec
        def test(n: Int): Boolean = {
            if (n >= as.length - 1) true
            else if (!ordered(as(n), as(n+1))) false
            else test(n+1)
        }

        test(0)
    }

    def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
       a => b => f(a,b)
    }

    def uncurry[A,B,C](f: A => B => C): (A,B) => C = {
        (a, b) => f(a)(b)
    }

    def compose[A,B,C](f: B => C, g: A => B): A => C = {
        a => f(g(a))
    }

    println(curry((a: Int,b: Int) => a > b)(1)(2))
}
