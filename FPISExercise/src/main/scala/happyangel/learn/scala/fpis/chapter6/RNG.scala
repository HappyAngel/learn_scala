package happyangel.learn.scala.fpis.chapter6


/**
  * Created by happyangel on 2018/6/30.
  * random number generator
  */

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object test extends App {
  type State[S, +A] = S => (A, S)
  type Rand[+A] = State[RNG, A]

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i+1) else i, r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    ((i.toDouble /Int.MaxValue), r)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, r1) = double(rng)
    val (i, r2) = r1.nextInt
    ((d, i), r2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)

    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def intsHelper(count: Int, res: List[Int])(rng: RNG): (List[Int], RNG) = {
      if (count == 0) {
        (res, rng)
      } else {
        val (i, r) = rng.nextInt
        intsHelper(count - 1, i :: res)(r)
      }
    }

    intsHelper(count, Nil)(rng)
  }

  def unit[A](a: A): Rand[A] = {
    rng => (a, rng)
  }

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  }

  def flatMap[A, B](s: Rand[A])(f: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, rng2) = s(rng)
      f(a)(rng2)
    }
  }

  def doubleNew: Rand[Double] = {
    map(nonNegativeInt) { i =>
      (i.toDouble / Int.MaxValue)
    }
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra) { a =>
      map(rb) { b =>
        f(a,b)
      }
    }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(Nil: List[A]))((x, res) =>
      map2(x, res)(_::_)
    )
  }

 def intsNew(count: Int): Rand[List[Int]] = {
    sequence(List.fill[Rand[Int]](count)(nonNegativeInt))
  }

  val tt = SimpleRNG(System.currentTimeMillis)
  println(doubleNew(tt))
}

case class State[S, +A](run: S => (A, S)) {
  import State._

  def map[B](f: A => B): State[S,B] = {
    flatMap{ a =>
      unit(f(a))
    }
  }

 def map2[B, C](rb: State[S, B])(f: (A, B) => C): State[S, C] = {
    flatMap { a =>
      rb.map { b =>
        f(a,b)
      }
    }
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    State (s => {
        val (a, ss) = run(s)
        f(a).run(ss)
      }
    )
  }
}

object State {
  def unit[S, A](a: A): State[S,A] = {
     State(s => (a, s))
  }

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
    fs.foldRight(unit[S, List[A]](Nil: List[A]))((x, res) =>
      x.map2(res)(_::_)
    )
  }

  def get[S]: State[S, S] = {
    State(s => (s, s))
  }

  def set[S](s: S): State[S, Unit] = {
    State(_ => ((), s))
  }
}
