package happyangel.learn.scala.fpis.chapter6

/**
  * Created by xionglei on 2018/7/15.
  */
case class State[S, +A](run: S => (A, S)) {
  import State._

  def map[B](f: A => B): State[S, B] = {
    flatMap { a =>
      unit(f(a))
    }
  }

  def map2[B, C](rb: State[S, B])(f: (A, B) => C): State[S, C] = {
    flatMap { a =>
      rb.map { b =>
        f(a, b)
      }
    }
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    State(s => {
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
