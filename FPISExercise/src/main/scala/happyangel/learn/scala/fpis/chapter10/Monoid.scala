package happyangel.learn.scala.fpis.chapter10

/**
  * Created by happyangel on 2018/9/1.
  */
trait Monoid[A]{
  // op(op(x,y),z) == op(x,op(y,z))
  def op(a1: A, a2: A): A
 // op(x, zero) == x and op(zero, x) == x
  def zero: A
}

object Main extends App {
  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String): String = {
      a1+a2
    }

    def zero: String = {
      ""
    }
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]): List[A] = {
      a1 ++ a2
    }

    def zero: List[A] = {
      Nil
    }
  }

  val intAddition = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = {
      a1 + a2
    }

    override def zero: Int = {
      0
    }
  }

  val intMultiplication = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = {
      a1 * a2
    }

    override def zero: Int = {
      1
    }
  }

  val booleanOr = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = {
      a1 || a2
    }

    override def zero: Boolean = {
      false
    }
  }

  val booleanAnd = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = {
      a1 && a2
    }

    override def zero: Boolean = {
      true
    }
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = {
      a1 orElse a2
    }

    override def zero: Option[A] = {
      None
    }
  }

  def endoMonoid[A]: Monoid[A=>A] = new Monoid[(A) => A] {
    override def op(a1: (A) => A, a2: (A) => A): (A) => A = {
      a1 andThen a2
    }

    override def zero: (A) => A = {
      a => a
    }
  }

  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B = {
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))
  }

  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (v.length < 1) m.zero
    else if (v.length == 1) {
      f(v(0))
    } else {
      val middleIndex = v.length/2
      val (a,b) = v.splitAt(middleIndex)
      m.op(foldMapV(a,m)(f), foldMapV(b,m)(f))
    }
  }

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = {
      (a1, a2) match {
        case (s1: Stub, s2: Stub) =>
          Stub(s1.chars+s2.chars)
        case (s1: Part, s2: Part) =>
          Part(s1.lStub, s1.words+s2.words + (if ((s1.rStub+s2.lStub).isEmpty) 0 else 1), s2.rStub)
        case (s1: Part, s2: Stub) =>
          Part(s1.lStub, s1.words, s1.rStub+s2.chars)
        case (s1: Stub, s2: Part) =>
          Part(s1.chars+s2.lStub, s2.words, s2.rStub)
      }
    }

    override def zero: WC = {
      Stub("")
    }
  }

  def countWordsInString(str: String): Int = {
    def wc(c: Char): WC = {
      if (c.isWhitespace) {
        Part("", 0, "")
      } else {
        Stub(c.toString)
      }
    }

    def unStub(s: String) = {
      s.length min 1
    }

    foldMapV(str, wcMonoid)(wc) match {
      case Stub(v) => unStub(v)
      case Part(l,w,r) => unStub(l) + w + unStub(r)
    }
  }
}
