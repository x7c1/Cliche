package x7c1.colorful.lib.chapter02

import scala.annotation.tailrec

object Exercise_2_1 {
  def fib(n: Int): Int = {
    @tailrec
    def loop(a: Int, b: Int, count: Int): Int = count match {
      case x if x <= 0 => a
      case _ => loop(b, a + b, count - 1)
    }
    loop(0, 1, n)
  }
}

object Exercise_2_2 {
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    as zip (as drop 1) forall ordered.tupled
  }
  def isSorted2[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(list: Seq[A]): Boolean = list match {
      case Seq() | _ +: Seq() => true
      case x1 +: x2 +: xs => ordered(x1, x2) && loop(xs)
    }
    loop(as)
  }
}

object Exercise_2_3 {
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a, b)
  }
}

object Exercise_2_4 {
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }
}

object Exercise_2_5 {
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }
}
