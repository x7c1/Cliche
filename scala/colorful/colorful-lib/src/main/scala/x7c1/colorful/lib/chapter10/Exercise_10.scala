package x7c1.colorful.lib.chapter10

import x7c1.colorful.lib.chapter08.{Gen, Prop}

import scala.annotation.tailrec

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  /* 10.5 */

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
    as.foldLeft(m.zero){(b, a) => m.op(b, f(a))}
  }

  /* 10.6 */

  def foldMap2[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
    as.foldRight(m.zero){(a, b) => m.op(f(a), b)}
  }

  /* 10.7 */

  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    @tailrec
    def loop(x: IndexedSeq[A], y: List[IndexedSeq[A]], z: B): B = (x, y) match {
      case (Seq(), _) => z
      case (Seq(a), _) =>
        val (head, tail) = (y.headOption.toIndexedSeq.flatten, y drop 1)
        loop(head, tail, m.op(z, f(a)))
      case _ =>
        val (head, tail) = x.splitAt(x.length / 2)
        loop(head, tail +: y, z)
    }
    loop(v, List(), m.zero)
  }

}

sealed trait WC

case class Stub(chars: String) extends WC

case class Part(lStub: String, words: Int, rStub: String) extends WC

object WC {

  /* 10.10 */

  val wcMonoid = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(chars), Part(l, words, r)) => Part(chars + l, words, r)
      case (Part(l, words, r), Stub(chars)) => Part(l, words, r + chars)
      case (Stub(chars1), Stub(chars2)) => Stub(chars1 + chars2)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
        val w = if ((r1 + l2).isEmpty) 0 else 1
        Part(l1, w1 + w + w2, r2)
    }
    override def zero: WC = Stub("")
  }
}

import scala.language.higherKinds
trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A,B) => B): B
  def foldLeft[A, B](as: F[A])(z: B)(f: (B,A) => B): B
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A

  /* 10.15 */
  def toList[A](fa: F[A]): List[A] = foldLeft(fa)(List[A]())(_ :+ _)
}

object Exercise_10_1 {

  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2
    override def zero: Int = 0
  }
  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2
    override def zero: Int = 1
  }
  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    override def zero: Boolean = false
  }
  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    override def zero: Boolean = true
  }
}

object Exercise_10_2 {
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
    override def zero: Option[A] = None
  }
}

object Exercise_10_3 {
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): A => A = a1 compose a2
    override def zero: A => A = a => a
  }
  def endoMonoid2[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): A => A = a1 andThen a2
    override def zero: A => A = a => a
  }
}

object Exercise_10_4 {
  def isMonoid[A](m: Monoid[A])(x: A, y: A, z: A): Boolean = {
    import m.{op, zero}
    val associativity = op(op(x, y), z) == op(x, op(y, z))
    val identity = { op(x, zero) == x } && { op(zero, x) == x }
    associativity && identity
  }
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    Prop.forAll(Gen.listOfN(3, gen)){
      case List(x, y, z) => isMonoid(m)(x, y, z)
    }
  }
}

object Exercise_10_5 {
  val stringMonoid = new Monoid[String] {
    override def op(a1: String, a2: String) = a1 + a2
    override val zero = ""
  }
}

object Exercise_10_6 {
  import Exercise_10_3.{endoMonoid, endoMonoid2}

  def foldRight[A, B](list: List[A], z: B)(f: (A, B) => B): B = {
    val endo = Monoid.foldMap(list, endoMonoid[B]){ a => b => f(a, b) }
    endo(z)
  }
  def foldLeft[A, B](list: List[A], z: B)(f: (B, A) => B): B = {
    val monoid = endoMonoid[B => B]
    val endo = Monoid.foldMap(list, monoid){ a => g => b => g(f(b, a)) }
    endo(b => b)(z)
  }

  /* under another endoMonoid implementation */

  def foldRight2[A, B](list: List[A], z: B)(f: (A, B) => B): B = {
    val monoid = endoMonoid2[B => B]
    val endo = Monoid.foldMap(list, monoid){ a => g => b => g(f(a, b)) }
    endo(b => b)(z)
  }
  def foldLeft2[A, B](list: List[A], z: B)(f: (B, A) => B): B = {
    val endo = Monoid.foldMap(list, endoMonoid2[B]){ a => b => f(b, a) }
    endo(z)
  }
}

object Exercise_10_8 {

  import fpinscala.parallelism.Nonblocking.Par

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    override def op(a1: Par[A], a2: Par[A]): Par[A] = Par.map2(a1, a2)(m.op)
    override def zero: Par[A] = Par.unit(m.zero)
  }

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
    @tailrec
    def loop(x: IndexedSeq[Par[A]], y: List[IndexedSeq[Par[A]]], z: Par[B]): Par[B] =
      (x, y) match {
        case (Seq(), _) => z
        case (Seq(a), _) =>
          val (head, tail) = (y.headOption.toIndexedSeq.flatten, y drop 1)
          loop(head, tail, par(m).op(z, Par.map(a)(f)))
        case _ =>
          val (head, tail) = x.splitAt(x.length / 2)
          loop(head, tail +: y, z)
      }

    loop(v.map(Par.unit), List(), par(m).zero)
  }

  // is it really executed in parallel?
  def parFoldMap2[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
    val f2: Par[A] => Par[B] = a => Par.map(a)(f)
    val v2: IndexedSeq[Par[A]] = v.map(a => Par.lazyUnit(a))
    Monoid.foldMapV(v2, par(m))(f2)
  }

  // how about this?
  def parFoldMap3[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
    val p: Par[IndexedSeq[B]] = Par.parMap(v)(f)
    Par.flatMap(p){ bs =>
      Monoid.foldMapV(bs, par(m)){ (b: B) => Par.lazyUnit(b) }
    }
  }
}

object Exercise_10_9 {
  type Piece = Option[(Int, Int, Boolean)]

  // from answer
  val comparatorMonoid = new Monoid[Piece] {
    override def op(a1: Piece, a2: Piece): Piece = {
      (a1, a2) match {
        case (Some((max1, min1, sorted1)), Some((max2, min2, sorted2))) =>
          Some((
            max1 max max2,
            min1 min min2,
            sorted1 && sorted2 && (max1 <= min2)))
        case (None, x)  => x
        case (x, None) => x
      }
    }
    override def zero: Piece = None
  }
  def isSorted(list: IndexedSeq[Int]): Boolean = {
    val piece = Monoid.foldMapV(list, comparatorMonoid){
      i => Some((i, i, true))
    }
    piece.exists(_._3)
  }

  type Piece2 = Option[(Int, Int, Boolean, Boolean)]

  // for the use of asc & desc
  val comparatorMonoid2 = new Monoid[Piece2] {
    override def op(a1: Piece2, a2: Piece2): Piece2 = {
      (a1, a2) match {
        case (Some((max1, min1, asc1, desc1)), Some((max2, min2, asc2, desc2))) =>
          Some((
            max1 max max2,
            min1 min min2,
            asc1 && asc2 && (max1 <= min2),
            desc1 && desc2 && (min1 >= max2)))
        case (None, x)  => x
        case (x, None) => x
      }
    }
    override def zero: Piece2 = None
  }

  def pieceFor(list: IndexedSeq[Int]): Piece2 =
    Monoid.foldMapV(list, comparatorMonoid2){
      i => Some((i, i, true, true))
    }

  def isAscendingOrdered(list: IndexedSeq[Int]): Boolean = {
    pieceFor(list).exists(_._3)
  }
  def isDescendingOrdered(list: IndexedSeq[Int]): Boolean = {
    pieceFor(list).exists(_._4)
  }

}

object Exercise_10_11 {
  def countWords(x: String): Int = {
    val wc = Monoid.foldMapV(x, WC.wcMonoid){
      case char if char == ' ' => Part("", 0, "")
      case char => Stub(char.toString)
    }
    def flatten(x: String) = if (x.isEmpty) 0 else 1

    wc match {
      case Stub(chars) => 1
      case Part(l, words, r) => flatten(l) + words + flatten(r)
    }
  }
}

object Exercise_10_12 {

  object ListFoldable extends Foldable[List]{
    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
      as.foldRight(z)(f)
    }
    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
      as.foldLeft(z)(f)
    }
    override def concatenate[A](as: List[A])(m: Monoid[A]): A = {
      as.foldLeft(m.zero)(m.op)
    }
    override def foldMap[A, B](as: List[A])(f: (A) => B)(mb: Monoid[B]): B = {
      Monoid.foldMap(as, mb)(f)
    }
  }

  /*
  it looks obvious without implementation :(
  object FoldableIndexedSeq extends Foldable[IndexedSeq]{
    foldMap -> foldMapV
    ...
  }
  object FoldableStream extends...
  */
}

object Exercise_10_13 {

  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object TreeFoldable extends Foldable[Tree] {

    override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as match {
      case Leaf(a) => f(a, z)
      case Branch(left, right) => foldRight(left)(foldRight(right)(z)(f))(f)
    }
    override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as match {
      case Leaf(a) => f(z, a)
      case Branch(left, right) => foldLeft(right)(foldLeft(left)(z)(f))(f)
    }
    override def concatenate[A](as: Tree[A])(m: Monoid[A]): A = {
      foldLeft(as)(m.zero)(m.op)
    }
    override def foldMap[A, B](as: Tree[A])(f: (A) => B)(mb: Monoid[B]): B =
      foldLeft(as)(mb.zero){ (b, a) => mb.op(b, f(a)) }
  }
}

object Exercise_10_14 {

  object OptionFoldable extends Foldable[Option] {

    override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B =
      as match {
        case Some(a) => f(a, z)
        case None => z
      }
    override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
      as match {
        case Some(a) => f(z, a)
        case None => z
      }
    override def concatenate[A](as: Option[A])(m: Monoid[A]): A =
      as match {
        case Some(a) => a
        case None => m.zero
      }
    override def foldMap[A, B](as: Option[A])(f: (A) => B)(mb: Monoid[B]): B =
      as match {
        case Some(a) => f(a)
        case None => mb.zero
      }
  }
}
