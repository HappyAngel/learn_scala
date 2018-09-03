package happyangel.learn.scala.fpis.chapter9

import scala.util.matching.Regex

/**
  * Created by happyangel on 2018/8/19.
  */
trait JSON
object JSON {

    case object JNull extends JSON

    case class JNumber(get: Double) extends JSON

    case class JString(get: String) extends JSON

    case class JBool(get: Boolean) extends JSON

    case class JArray(get: IndexedSeq[JSON]) extends JSON

    case class JObject(get: Map[String, JSON]) extends JSON

    def jsonParser[Parser[+ _]](P: Parsers[Parser]): Parser[JSON] = {
        import happyangel.learn.scala.fpis.chapter9.Parsers._

        def array = surround
    }
}


trait Parsers[Parser[+ _]] { self =>
    // primitive api choose between two parsers, first attempting p1, and then p2 if p1 fails
    def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]
    // primitive api
    def succeed[A](a: A): Parser[A]
    // primitive api: recognize and returns a single string
    implicit def string(s: String): Parser[String]
    // primitive api: return the portion of the input string examined by the parser
    def slice[A](p: Parser[A]): Parser[String]
    // primitive api: used for context-sensitive grammars
    def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]
    // primitive api: promotes a regular expression to be a Parser
    implicit def regex(r: Regex): Parser[String]

    def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
        if (n <= 0) {
            succeed(List())
        } else {
            map2(p, listOfN(n - 1, p))(_ :: _)
        }
    }

    def many[A](p: Parser[A]): Parser[List[A]] = {
        or(map2(p, many(p))(_ :: _), succeed(List()))
    }

    // recognize one or more x character
    def many1[A](p: Parser[A]): Parser[List[A]] = {
        map2(p, many(p))(_ :: _)
    }

    def map[A, B](a: Parser[A])(f: A => B): Parser[B] = {
        flatMap(a) { aa =>
           succeed(f(aa))
        }
    }

    def char(c: Char): Parser[Char] = map(string(c.toString))(_.charAt(0))

    def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] = {
        for {
            a <- p
            b <- p2
        } yield {
            (a, b)
        }
    }

    def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = {
        for {
            a <- p
            b <- p2
        } yield {
            f(a,b)
        }
    }

    def skipL[B](p: Parser[Any], p2: => Parser[B]): Parser[B] = {
        map2(slice(p), p2)((_, b) => b)
    }

    def skipR[A](p: Parser[A], p2: => Parser[Any]): Parser[A] = {
        map2(p, slice(p2))((a,_) => a)
    }

    def surround[A](start: Parser[Any], stop: Parser[Any])(p: => Parser[A]) = {
        start
    }

    implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

    implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))


    case class ParserOps[A](p: Parser[A]) {
        def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

        def **[B >: A](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

        def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

        def *>[B](p2: => Parser[B]) = self.skipL(p, p2)

        def <*[B](p2: => Parser[B]) = self.skipR(p, p2)
    }

}
