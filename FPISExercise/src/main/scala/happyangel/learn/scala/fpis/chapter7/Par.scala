package happyangel.learn.scala.fpis.chapter7

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

import happyangel.learn.scala.fpis.chapter7.Par.Par

/**
  * Created by xionglei on 2018/7/29.
  */

object Par {
    type Par[T] = ExecutorService => Future[T]

    private case class UnitFuture[T](get: T) extends Future[T] {
        def isDone: Boolean = true

        def get(timeout: Long, unit: TimeUnit): T = get

        def isCancelled: Boolean = false

        def cancel(mayInterruptIfRunning: Boolean): Boolean = false
    }
    // return a computation that may evaluate in a separate thread
    def unit[T](a: T): Par[T] = (_: ExecutorService) => UnitFuture(a)
    def lazyUnit[T](a: => T): Par[T] = fork(unit(a))

    // spawning parallel computations and extract the result
    def run[T](a: Par[T]): T = ???

    def fork[T](a: => Par[T]): Par[T] = {
        es => es.submit(new Callable[T] {
            def call = a(es).get
        })
    }

    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = {
        (es: ExecutorService) => {
            val af = a(es)
            val bf = b(es)
            UnitFuture(f(af.get, bf.get))
        }
    }

    def map[A, B](pa: Par[A])(f: A => B): Par[B] = {
        map2(pa, unit()) { (a, _) =>
          f(a)
        }
    }

    def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
        ps.foldRight(unit[List[A]](Nil)) { (a, z) =>
            map2(a, z)(_::_)
        }
    }

    def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
        val fbs = ps.map(test.asyncF(f))
        sequence(fbs)
    }

    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
        sequence(as.filter(f).map(unit))
    }
}

object test extends App {
    def asyncF[A, B](f: A => B): (A => Par[B]) = {
        a => {
            Par.lazyUnit(f(a))
        }
    }
}

