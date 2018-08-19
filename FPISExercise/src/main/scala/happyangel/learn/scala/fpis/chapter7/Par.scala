package happyangel.learn.scala.fpis.chapter7

import java.util.concurrent._
import java.util.concurrent.atomic.AtomicReference

import happyangel.learn.scala.fpis.chapter7.Par.Par

/**
  * Created by happyangel on 2018/7/29.
  */

sealed trait AsyncFuture[A] {
    private[chapter7] def apply(k: A => Unit): Unit
}

object Par {
    type Par[T] = ExecutorService => AsyncFuture[T]

    // return a computation that may evaluate in a separate thread
    def unit[T](a: T): Par[T] =  {
        _ => {
            new AsyncFuture[T] {
                override private[chapter7] def apply(k: (T) => Unit) = {
                    k(a)
                }
            }
        }
    }
    def lazyUnit[T](a: => T): Par[T] = fork(unit(a))

    // spawning parallel computations and extract the result
    def run[T](es: ExecutorService)(a: Par[T]): T = {
        val ref = new AtomicReference[T]
        val latch = new CountDownLatch(1)
        a(es) { t =>
            ref.set(t)
            latch.countDown()
        }
        latch.await()
        ref.get()
    }

    def fork[T](a: => Par[T]): Par[T] = {
        es => new AsyncFuture[T] {
            override private[chapter7] def apply(k: (T) => Unit) = {
                eval(es)(a(es)(k))
            }
        }
    }

    def eval(es: ExecutorService)(r: => Unit): Unit = {
        es.submit(new Callable[Unit] {
            override def call(): Unit = r
        })
    }

    def delay[T](a: => Par[T]): Par[T] = {
        es => a(es)
    }
}

object test extends App {
    def asyncF[A, B](f: A => B): (A => Par[B]) = {
        a => {
            Par.lazyUnit(f(a))
        }
    }

    def hugeCompute = {
        for (i <- 1 to Int.MaxValue) {
            print(Thread.currentThread().getName)
            Thread.sleep(1000)
        }
    }

    print(Thread.currentThread().getName)
}

