package happyangel.learn.scala.common

import sun.security.util.Length

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.ArrayBuffer
import scala.collection.{IndexedSeqLike, mutable}

/**
  * Created by LeiXiong on 2017/8/13.
  *
  * to demonstrate the collection ability
  */
abstract class Base
case object A extends Base
case object T extends Base
case object G extends Base
case object U extends Base
object Base {
  val fromInt: Int => Base = Array(A, T, G, U)
  val toInt: Base => Int = Map(A->0, T->1, G->2, U->3)
}
//final class RNA1 private(val groups: Array[Int], val length: Int) extends IndexedSeq[Base] {
//  import RNA1._
//  override def apply(idx: Int): Base = {
//    if (idx < 0 || length <= idx) {
//      throw new IndexOutOfBoundsException
//    }
//
//    Base.fromInt(groups(idx / N) >> (idx % N * S) & M)
//  }
//}

// with IndexedSeqLike in order to get the 'same result type', meaning filer, take and other transform
// method should return the same type as its input
final class RNA2 private(val groups: Array[Int], val length: Int) extends IndexedSeq[Base] with IndexedSeqLike[Base, RNA2] {
  import RNA2._

  override protected[this] def newBuilder: mutable.Builder[Base, RNA2] = RNA2.newBuilder

  def apply(idx: Int): Base = {
        if (idx < 0 || length <= idx) {
          throw new IndexOutOfBoundsException
        }

        Base.fromInt(groups(idx / N) >> (idx % N * S) & M)
  }
}

object RNA2 {
  private val S = 2
  private val N = 32/S
  // BitMask to isolate a group
  private val M = (1<<S) - 1

  def fromSeq(buf: Seq[Base]): RNA2 = {
    val groups = new Array[Int]((buf.length + N - 1) / N)
    for (i <- 0 until buf.length) {
      groups(i/N) |= Base.toInt(buf(i)) << (i%N*S)
    }
    new RNA2(groups, buf.length)
  }

  def apply(bases: Base*) = fromSeq(bases)

  def newBuilder: mutable.Builder[Base, RNA2] = new ArrayBuffer[Base] mapResult fromSeq
  implicit def canBuildFrom: CanBuildFrom[RNA2, Base, RNA2] = new CanBuildFrom[RNA2, Base, RNA2] {
    def apply() = newBuilder
    def apply(from: RNA2) = newBuilder
  }
}
