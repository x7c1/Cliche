package x7c1.colorful.lib.chapter03

import fpinscala.datastructures.List.{append, foldRight, sum}
import fpinscala.datastructures.{Cons, List, Nil}
import x7c1.colorful.lib.chapter03.Exercise_3_10.foldLeft
import x7c1.colorful.lib.chapter03.Exercise_3_12.reverse

import scala.annotation.tailrec

object Exercise_3_1 {
  def call() = {
    List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
  }
}

object Exercise_3_2 {
  def tail[A](xs: List[A]): List[A] =
    xs match {
      case Cons(_, tail) => tail
      case Nil =>
        throw new UnsupportedOperationException("tail of empty list")
    }

  /*
    memo) built-in Scala's Nil.tail behaves like:
    scala> Nil.tail
    java.lang.UnsupportedOperationException: tail of empty list
  */
}

object Exercise_3_3 {
  def setHead[A](xs: List[A], value: A): List[A] =
    xs match {
      case Cons(_, tail) => Cons(value, tail)
      case Nil =>
        throw new UnsupportedOperationException("setHead of empty list")
    }

  /*
    memo) built-in Scala's Nil.head behaves like:
    scala> Nil.head
    java.util.NoSuchElementException: head of empty list
  */
}

object Exercise_3_4 {
  @tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    (l, n) match {
      case (_, x) if x <= 0 => l
      case (Nil, _) => l
      case (Cons(_, tail), _)  => drop(tail, n - 1)
    }
}

object Exercise_3_5 {
  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(x, xs) if f(x) => dropWhile(xs, f)
      case _ => l
    }
}

object Exercise_3_6 {
  def init[A](l: List[A]): List[A] = {
    @tailrec
    def loop(list: List[A], y: List[A] = List()): List[A] =
      list match {
        case Cons(x, Nil) => y
        case Cons(x, xs) => loop(xs, Cons(x, y))
        case Nil =>
          throw new UnsupportedOperationException("empty.init")
      }
    @tailrec
    def reverse(list: List[A], y: List[A] = List()): List[A] =
      list match {
        case Nil => y
        case Cons(x, xs) => reverse(xs, Cons(x, y))
      }

    reverse(loop(l))
  }

  /*
    memo) built-in Scala's Nil.init behaves like:
    scala> Nil.init
    java.lang.UnsupportedOperationException: empty.init
   */
}

object Exercise_3_7 {
  /*
    No. product cannot halt in recursion.

    `f(x, foldRight(xs, z))`

    f requires the value returned by foldRight on current
    recursive implementation. foldRight must traverse all
    the way to the end of the list.
   */
}

object Exercise_3_8 {
  def call() = {
    foldRight(List(1, 2, 3), Nil:List[Int])(Cons(_, _))
  }
}

object Exercise_3_9 {
  def length[A](as: List[A]): Int = {
    foldRight(as, 0){ (_, count) => count + 1 }
  }
}

object Exercise_3_10 {
  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
      case Nil => z
    }
}

object Exercise_3_11 {

  def sum(ints: List[Int]): Int =
    foldLeft(ints, 0){_ + _}

  def product(ds: List[Double]): Double =
    foldLeft(ds, 1.0){_ * _}

  def length[A](as: List[A]): Int = {
    foldLeft(as, 0){ (count, _) => count + 1 }
  }
}

object Exercise_3_12 {

  def reverse[A](list: List[A]): List[A] = {
    foldLeft(list, List[A]()){ (xs, x) => Cons(x, xs) }
  }
}

object Exercise_3_13 {

  def id[A] = (x: A) => x

  def foldLeft2[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(as), z){ (a, b) => f(b, a) }

  def foldLeft3[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(as, id[B]){ (a, g) => b => g(f(b, a)) }(z)

  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z){ (b, a) => f(a, b) }

  def foldRight3[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(as, id[B]){ (g, a) => b => g(f(a, b)) }(z)
}

object Exercise_3_14 {

  def append2[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2){ Cons(_, _) }
  }
  def append3[A](a1: List[A], a2: List[A]): List[A] = {
    def id = (x: List[A]) => x
    foldLeft(a1, id){ (g, x) => xs => g(Cons(x, xs)) }(a2)
  }
}

object Exercise_3_15 {
  import Exercise_3_14.append2

  def appendAll[A](list: List[List[A]]): List[A] = {
    foldRight(list, List[A]())(append2)
  }
}

object Exercise_3_16 {
  def addOne(list: List[Int]): List[Int] = {
    foldRight(list, List[Int]()){ (x, xs) => Cons(x + 1, xs) }
  }
}

object Exercise_3_17 {
  def convertToString(list: List[Double]): List[String] = {
    foldRight(list, List[String]()){ (x, xs) => Cons(x.toString, xs) }
  }
}

object Exercise_3_18 {
  def map[A, B](as: List[A])(f: A => B): List[B] = {
    foldRight(as, List[B]()){ (x, xs) => Cons(f(x), xs) }
  }
}

object Exercise_3_19 {
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, List[A]()){
      case (x, xs) if f(x) => Cons(x, xs)
      case (_, xs) => xs
    }
  }
}

object Exercise_3_20 {
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    foldRight(as, List[B]()){ (x, xs) => append(f(x), xs) }
  }
}

object Exercise_3_21 {
  import Exercise_3_20.flatMap

  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as){
      case x if f(x) => List(x)
      case _ => Nil
    }
}

object Exercise_3_22 {
  def add(as: List[Int], bs: List[Int]): List[Int] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, add(xs, ys))
  }
}

object Exercise_3_23 {
  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] =
    (as, bs) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
    }
}

object Exercise_3_24 {
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @tailrec
    def startsWith(as: List[A], bs: List[A]): Boolean =
      (as, bs) match {
        case (Nil, Nil) | (_, Nil) => true
        case (Nil, _) => false
        case (Cons(x, xs), Cons(y, ys)) => (x == y) && startsWith(xs, ys)
      }
    @tailrec
    def loop(as: List[A]): Boolean =
      as match {
        case _ if startsWith(as, sub) => true
        case Nil => false
        case Cons(_, tail) => loop(tail)
      }
    loop(sup)
  }
}

object Exercise_3_25 {
  import fpinscala.datastructures.{Branch, Leaf, Tree}

  def size[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }
  }
}

object Exercise_3_26 {
  import fpinscala.datastructures.{Branch, Leaf, Tree}

  def maximum(tree: Tree[Int]): Int = {
    tree match {
      case Leaf(x) => x
      case Branch(left, right) => maximum(left) max maximum(right)
    }
  }
}

object Exercise_3_27 {
  import fpinscala.datastructures.{Branch, Leaf, Tree}

  def depth(tree: Tree[Int]): Int = {
    tree match {
      case Leaf(_) => 0
      case Branch(left, right) => 1 + (depth(left) max depth(right))
    }
  }
}

object Exercise_3_28 {
  import fpinscala.datastructures.{Branch, Leaf, Tree}

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    tree match {
      case Leaf(x) => Leaf(f(x))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }
  }
}

object Exercise_3_29 {
  import fpinscala.datastructures.{Branch, Leaf, Tree}

  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = {
    tree match {
      case Leaf(x) => f(x)
      case Branch(left, right) =>  g(fold(left)(f)(g), fold(right)(f)(g))
    }
  }
  def size[A](tree: Tree[A]): Int = {
    fold(tree)(_ => 1)(1 + _ + _)
  }
  def maximum(tree: Tree[Int]): Int = {
    fold(tree)(x => x)(_ max _)
  }
  def depth(tree: Tree[Int]): Int = {
    fold(tree)(_ => 0){(a, b) => 1 + (a max b)}
  }
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    val g: A => Tree[B] = x => Leaf(f(x))
    fold(tree)(g)(Branch(_, _))
  }
}