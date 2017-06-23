package happyangel.learn.scala.common

/**
  * Created by root on 17-6-5.
  */

// n, d are class parameters, and are private, which will be used to create the primary constructor
class Rational(n: Int, d: Int) extends Ordered[Rational]{
    // preconditions
    require(d!=0)

    private val g = gcd(n.abs, d.abs)
    val numer: Int = n / g // fields are public
    val denom: Int = d / g

    // auxiliary constructor, must explicitly call other auxiliary constructors or the primary constructor as first action
    def this(n: Int) = this(n, 1)

    override def toString = numer + "/" + denom

    def +(that: Rational): Rational = {
        new Rational(
            numer * that.denom + that.numer * denom,
            denom * that.denom
        )
    }

    def *(that: Rational): Rational = {
        new Rational(numer * that.numer, denom * that.denom)
    }

    private def gcd(a: Int, b: Int): Int = if (b==0) a else gcd(b, a%b)

    override def compare(that: Rational): Int = {
        (this.numer * that.denom) - (this.denom * that.numer)
    }
}

object app extends App {
    println(new Rational(1, 2) <= new Rational(1,1))
}
