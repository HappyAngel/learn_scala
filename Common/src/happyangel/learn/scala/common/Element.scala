package happyangel.learn.scala.common

/**
  * Created by xionglei on 17-6-9.
  *
  * from The Scala programming language Ch10
  */
abstract class Element {
    def contents: Array[String]

    def height: Int = contents.length

    def width: Int = if (height == 0) 0 else contents(0).length
}

class ArrayElement(conts: Array[String]) extends Element {
   val contents: Array[String] = conts
}

