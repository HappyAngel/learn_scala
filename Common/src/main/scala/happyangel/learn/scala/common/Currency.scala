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
abstract class Currency {
  val amount: Long
  def designation: String

  override def toString: String = amount + " " + designation

  def + (that: Currency): Currency = {
    amount + that.amount
  }
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

