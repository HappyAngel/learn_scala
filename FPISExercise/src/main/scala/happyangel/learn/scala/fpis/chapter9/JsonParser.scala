package happyangel.learn.scala.fpis.chapter9

/**
  * Created by happyangel on 2018/8/19.
  */
object JsonParser {

}

trait Parsers[ParseError, Parser[+_]] { self =>
  // primitive api choose between two parsers, first attempting p1, and then p2 if p1 fails
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] = ???
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
    if (n <= 0) {
      succeed(List())
    } else {
      map2(p, listOfN(n-1,p))(_::_)
    }
  }

  def many[A](p: Parser[A]): Parser[List[A]] = {
    or(map2(p, many(p))(_::_), succeed(List()))
  }

  // recognize one or more x character
  def many1[A](p: Parser[A]): Parser[List[A]] = {
    map2(p, many(p))(_::_)
  }
  def map[A, B](a: Parser[A])(f: A => B): Parser[B]

  // primitive api recognize and returns a single string
  implicit def string(s: String): Parser[String]
  def char(c: Char): Parser[Char] = map(string(c.toString))(_.charAt(0))
  def succeed[A](a: A): Parser[A] = map(string(""))(_ => a)
  // primitive api return the portion of the input string examined by the parser
  def slice[A](p: Parser[A]): Parser[String]
  // primitive api sequences two parsers, running p1 and then p2, and returns the pair of their results if both succeed
  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)]
  // primitive api: used for context-sensitive grammars
  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def map2[A, B, C](p: Parser[A],p2: => Parser[B])(f: (A,B) => C): Parser[C] = {
    map(product(p, p2))(a => f(a._1,a._2))
  }

  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))


  case class ParserOps[A](p: Parser[A]) {
    def | [B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def ** [B>:A](p2: Parser[B]): Parser[(A,B)] = self.product(p, p2)
    def or [B>:A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
  }
}
