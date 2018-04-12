package happyangel.learn.scala.common

import java.security.MessageDigest
import java.util.Base64

import com.fasterxml.jackson.databind.{DeserializationFeature, ObjectMapper, PropertyNamingStrategy}
import com.google.gson.{Gson, GsonBuilder}
import net.sf.json.JSONObject
import play.api.libs.json.{JsObject, Json}

/**
  * Created by happyangel on 2017/7/15.
  *
  *  to demonstrate The Scala programming language ch20
  */
abstract class CurrencyZone {
    type Currency <: AbstractCurrency
    def make(amount: Long): Currency

    abstract class AbstractCurrency {
        val amount: Long

        def designation: String

        def +(that: Currency): Currency = {
            make(amount + that.amount)
        }

        def *(x: Double): Currency = {
            make((amount * x).toLong)
        }

        def -(that: Currency): Currency = {
            make(this.amount - that.amount)
        }

        def / (that: Double) = {
            make((this.amount / that).toLong)
        }

        def / (that: Currency) = {
            this.amount.toDouble / that.amount
        }

        def from(other: CurrencyZone#AbstractCurrency): Currency = {
            make(math.round(other.amount.toDouble * Converter.exchangeRate(other.designation)(this.designation)))
        }

        override def toString =
            ((amount.toDouble / CurrencyUnit.amount.toDouble) formatted("%." + decimals(CurrencyUnit.amount) + "f") + " " + designation)

        private def decimals(n: Long): Int = {
            if (n==1) 0 else 1 + decimals(n/10)
        }
    }

    val CurrencyUnit: Currency
}

object US extends CurrencyZone {
    type Currency = Dollar
    abstract class Dollar extends AbstractCurrency {
        type Currency = Dollar
        def designation = "USD"
    }

    override def make(x: Long) = new Dollar {
        override val amount: Long = x
    }

    val Cent = make(1)
    val Dollar = make(100)
    val CurrencyUnit = Dollar
}

object Europe extends CurrencyZone {
    abstract class Euro extends AbstractCurrency {
        override def designation = "EUR"
    }

    override type Currency = Euro
    def make(cents: Long) = new Euro {
        val amount = cents
    }

    val Cent = make(1)
    val Euro = make(100)
    val CurrencyUnit = Euro
}

object Converter {
    var exchangeRate = Map (
        "USD" -> Map("USD" -> 1.0, "EUR" -> 0.756, "JPY" -> 1.211, "CHY" -> 1.233),
        "EUR" -> Map("EUR" -> 1.0, "USD" -> 1.316)
    )
}

object test1 extends App {
    val ll = List("1","2")

    println("3" :: ll)
}

class TestClass {
    var idd = ""
    def setId(id: String): TestClass = {
        idd = id
        this
    }

    override def toString: String = idd
}

