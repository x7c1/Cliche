package x7c1.colorful.lib.chapter12

import x7c1.colorful.lib.chapter11.{Monad, Functor}

import scala.language.{reflectiveCalls, higherKinds}

trait Applicative[F[_]]
  extends Listing_12_1[F]
    with Exercise_12_1[F]
    with Exercise_12_2[F]
    with Exercise_12_3[F]
    with Exercise_12_8[F]
    with Exercise_12_9[F]

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
          G.map(G.product(ga, gb))(f.tupled)

        self.map2(fa, fb)(g)
      }
      override def unit[A](a: => A): F[G[A]] = self unit G.unit(a)
    }
}
