package x7c1.colorful.lib.chapter12

import x7c1.colorful.lib.chapter11.Functor

import scala.language.higherKinds

trait Applicative[F[_]]
  extends Listing_12_1[F]
    with Exercise_12_1[F]
    with Exercise_12_2[F]
    with Exercise_12_3[F]

trait Listing_12_1[F[_]] extends Functor[F]{

  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

  def unit[A](a: => A): F[A]

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
