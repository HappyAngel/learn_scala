package happyangel.learn.scala.common

import com.google.gson.{FieldNamingPolicy, Gson, GsonBuilder}

/**
  * Created by xionglei on 17-6-26.
  *
  * The scala programming language ch16
  *
  */
class ListExpand {

    def append[T] (xs: List[T], ys: List[T]): List[T] =
        xs match {
            case List() => ys
            case x :: xs1 => x :: append(xs1, ys)
        }

    def msort[T](xs: List[T])(less: (T,T) => Boolean): List[T] = {
        def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
            case (Nil, _) => ys
            case (_, Nil) => xs
            case (x :: xs1, y :: ys1) =>
                if (less(x,y)) x::merge(xs1, ys)
                else y :: merge(xs, ys1)
        }

        val n = xs.length / 2
        if (n == 0) xs
        else {
            val (ys, zs) = xs splitAt n
            merge(msort(ys)(less), msort(zs)(less))
        }
    }
}


object Test extends App {
    val test = """  this is "test"   """

    println(new GsonBuilder().setFieldNamingPolicy(FieldNamingPolicy.LOWER_CASE_WITH_UNDERSCORES).create.toJson(test))
}