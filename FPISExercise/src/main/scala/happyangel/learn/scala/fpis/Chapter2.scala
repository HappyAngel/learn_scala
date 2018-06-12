package happyangel.learn.scala.fpis

/**
  * Created by xionglei on 18-4-12.
  */
object Chapter2 extends App {
    def fib(n: Int): Int = {
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


    println(302002000 < 303000000)
}
