package happyangel.learn.scala.common

import scala.collection.mutable.ArrayBuffer

/**
  * Created by xionglei on 17-6-12.
  *
  * from The Scala programming language ch12.5 to demonstrate traits stackable modification
  */
abstract class IntQueue {
    def get(): Int
    def put(x: Int)
}

class BasicIntQueue extends IntQueue {
    private val buf = new ArrayBuffer[Int]()
    def get() = buf.remove(0)
    def put(x: Int) = { buf += x }
}

// Doubling can only mix in those classes that extend IntQueue, and it's dynamic binding
trait Doubling extends IntQueue {
    abstract override def put(x: Int) = { super.put(2*x) }
}

// traits further to the right is taken first
trait Incrementing extends IntQueue {
    abstract override def put(x: Int) = { super.put(x+1) }
}

trait Filtering extends IntQueue {
    abstract override def put(x: Int) = {
        if (x >= 0) super.put(x)
    }
}

