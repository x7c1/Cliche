package x7c1.colorful.lib.chapter12

import x7c1.colorful.lib.chapter06.State
import x7c1.colorful.lib.chapter10.{Monoid, Foldable}
import x7c1.colorful.lib.chapter11.{Monad, Exercise_11_2, Functor}

import scala.language.{reflectiveCalls, higherKinds}

trait Applicative[F[_]]
  extends Listing_12_1[F]
    with Exercise_12_1[F]
    with Exercise_12_2[F]
    with Exercise_12_3[F]
    with Exercise_12_8[F]
    with Exercise_12_9[F]
    with Exercise_12_12[F]

trait Listing_12_1[F[_]] extends Functor[F]{

  def unit[A](a: => A): F[A]

  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

  override def map[A,B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(()))((a, _) => f(a))

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] = {
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))
  }
}

trait Exercise_12_1[F[_]]{
  self: Listing_12_1[F] =>

  def sequence[A](lma: List[F[A]]): F[List[A]] = {
    val init: F[List[A]] = unit(List[A]())
    lma.foldRight(init){ (fa, acc) => map2(fa, acc){_ :: _} }
  }

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = {
    sequence(List.fill(n)(ma))
  }

  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = {
    map2(ma, mb)((_, _))
  }
}

trait Exercise_12_2[F[_]] {
  self: Exercise_12_1[F] =>

  def unit[A](a: => A): F[A]

  /* implement (map & map2) by (unit & apply) */

  def map_[A,B](fa: F[A])(f: A => B): F[B] = apply(unit[A => B](f))(fa)

  def map2_[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    val f2: F[A => B => C] = unit(a => b => f(a, b))
    val bc: F[B => C] = apply(f2)(fa)
    apply(bc)(fb)
  }

  /* implement (apply) by (unit & map2) */

  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] = {
    map2_(fab, fa){ (f, a) => f(a) }
  }
}

trait Exercise_12_3[F[_]] {
  self: Exercise_12_2[F] =>

  def map3[A,B,C,D](fa: F[A], fb: F[B], fc: F[C])
    (f: (A, B, C) => D): F[D] = {

    val f2: F[A => B => C => D] = unit(f.curried)
    apply(apply(apply(f2)(fa))(fb))(fc)
  }

  def map4[A,B,C,D,E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])
    (f: (A, B, C, D) => E): F[E] = {

    val f2: F[A => B => C => D => E] = unit(f.curried)
    apply(apply(apply(apply(f2)(fa))(fb))(fc))(fd)
  }
}

object Exercise_12_4 {
  val streamApplicative = new Applicative[Stream] {
    def unit[A](a: => A): Stream[A] = Stream.continually(a)
    def map2[A,B,C](a: Stream[A], b: Stream[B])(f: (A,B) => C): Stream[C] =
      a zip b map f.tupled
  }
}

object Exercise_12_5 {
  def eitherMonad[E] = new Monad[({type f[x] = Either[E, x]})#f] {
    override def flatMap[A, B](ma: Either[E, A])
      (f: A => Either[E, B]): Either[E, B] = ma.right.flatMap(f)

    override def unit[A](a: => A): Either[E, A] = Right(a)
  }
}

sealed trait Validation[+E, +A]

case class Failure[E](
  head: E,
  tail: Vector[E] = Vector()) extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]

object Exercise_12_6 {
  def validationApplicative[E] =
    new Applicative[({type f[x] = Validation[E, x]})#f] {

      override def unit[A](a: => A): Validation[E, A] = Success(a)

      override def map2[A, B, C]
        (fa: Validation[E, A], fb: Validation[E, B])
        (f: (A, B) => C): Validation[E, C] = (fa, fb) match {

        case (Success(a), Success(b)) => Success(f(a, b))
        case (Success(a), f: Failure[E]) => f
        case (f: Failure[E], Success(b)) => f
        case (Failure(h1, t1), Failure(h2, t2)) => Failure(h1, t1 ++ (h2 +: t2))
      }

    }
}

trait Exercise_12_8[F[_]]{
  self: Applicative[F] =>

  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    new Applicative[({type f[x] = (F[x], G[x])})#f] {

      override def map2[A, B, C]
        (fa: (F[A], G[A]), fb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) = {

        val ((f1, g1), (f2, g2)) = (fa, fb)
        val fc: F[C] = self.map2(f1, f2)(f)
        val gc: G[C] = G.map2(g1, g2)(f)
        (fc, gc)
      }

      override def unit[A](a: => A): (F[A], G[A]) = (self unit a, G unit a)
    }
  }
}

trait Exercise_12_9[F[_]]{
  self: Applicative[F] =>

  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] =
    new Applicative[({type f[x] = F[G[x]]})#f] {
      override def map2[A, B, C]
        (fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] = {

        val g: (G[A], G[B]) => G[C] = (ga, gb) =>
          G.map2(ga, gb)(f)

        self.map2(fa, fb)(g)
      }
      override def unit[A](a: => A): F[G[A]] = self unit G.unit(a)
    }
}

object Exercise_12_10 {
  /*
   :(
     https://github.com/fpinscala/fpinscala/blob/6c70d60d4f63e7823070406030c8876c6990abbe/answerkey/applicative/10.hint.txt#L1
     https://github.com/fpinscala/fpinscala/blob/6c70d60d4f63e7823070406030c8876c6990abbe/answerkey/applicative/10.answer.scala#L2-L3
     https://github.com/runarorama/sannanir/blob/master/Applicative.v
   */
}

trait Exercise_12_11 [F[_]]{
  self: Monad[F] =>

  def compose[G[_]](G: Monad[G]): Monad[({type f[x] = F[G[x]]})#f] =
    new Monad[({type f[x] = F[G[x]]})#f] {
      override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

      override def flatMap[A, B](ma: F[G[A]])(f: A => F[G[B]]): F[G[B]] = {
        /*
        self.flatMap(ma){a => x(a)}
          x requires `G[A] => F[G[B]]`, though `A => F[G[B]]` cannot be it.

        self.map(ma){a => x(a)}
          In this case, similarly, x requires unreachable `G[A] => G[B]`
        */
        ???
      }
    }
}

trait Exercise_12_12[F[_]] {
  self: Applicative[F] =>

  def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] = {
    val init = unit(Map[K, V]())
    ofa.foldRight(init){case ((k, fv), fmap) =>
      map2(fv, fmap){(v, map) => map + (k -> v) }
    }
  }
}

trait Traverse[F[_]]
  extends Functor[F]
  with Foldable[F] with FoldableImpl[F]
  with Exercise_12_14[F]
  with Listing_12_12[F]
  with Listing_12_15[F]
{
  def traverse[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))

  def sequence[G[_]:Applicative,A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(ga => ga)
}

case class Tree[+A](head: A, tail: List[Tree[A]])

object Exercise_12_13 {
  val listTraverse = new Traverse[List] {
    override def traverse[G[_] : Applicative, A, B]
      (fa: List[A])(f: A => G[B]): G[List[B]] = {

      val g = implicitly[Applicative[G]]
      val init: G[List[B]] = g.unit(List[B]())
      fa.foldRight(init){(a, acc) => g.map2(f(a), acc){_ :: _} }
    }

    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

  val optionTraverse = new Traverse[Option] {
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa map f

    override def traverse[G[_] : Applicative, A, B]
      (fa: Option[A])(f: A => G[B]): G[Option[B]] = {

      val g = implicitly[Applicative[G]]
      fa match {
        case Some(a) => g.map(f(a))(Some(_))
        case None => g unit None
      }
    }
  }

  val treeTraverse = new Traverse[Tree] {
    override def traverse[G[_] : Applicative, A, B]
      (fa: Tree[A])(f: A => G[B]): G[Tree[B]] = {

      val tail: G[List[Tree[B]]] =
        listTraverse.traverse(fa.tail){
          ta => traverse(ta)(f)
        }
      val head: G[B] = f(fa.head)
      implicitly[Applicative[G]].map2(head, tail){(h, t) => Tree(h, t)}
    }

    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = {
      Tree(f(fa.head), fa.tail.map(map(_)(f)))
    }
  }
}

trait Exercise_12_14[F[_]] {
  self: Traverse[F] =>

  override def map[A, B](fa: F[A])(f: A => B): F[B] = {

    type Foo[X] = X

    implicit object x extends Monad[Foo] {
      override def unit[A](a: => A): A = a

      override def flatMap[A, B](ma: A)(f: A => B): B = f(ma)
    }
    traverse[Foo, A, B](fa)(f)
  }
}

trait Listing_12_12[F[_]] {
  self: Traverse[F] =>

  def traverseS[S,A,B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S,x]})#f,A,B](fa)(f)(Monad.stateMonad)
}

trait Listing_12_15[F[_]] {
  self: Traverse[F] =>

  import State.{get, set}

  def mapAccum[S,A,B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => for {
      s1 <- get[S]
      (b, s2) = f(a, s1)
      _ <- set(s2)
    } yield b).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1
}

trait FoldableImpl[F[_]] {
  self: Foldable[F] =>

  override def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B = ???

  override def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B = ???

  override def concatenate[A](as: F[A])(m: Monoid[A]): A = ???

  override def foldMap[A, B](as: F[A])(f: (A) => B)(mb: Monoid[B]): B = ???
}

