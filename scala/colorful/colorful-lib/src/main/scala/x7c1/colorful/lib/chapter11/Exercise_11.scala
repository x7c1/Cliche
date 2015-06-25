package x7c1.colorful.lib.chapter11

import fpinscala.parallelism.Par
import fpinscala.parallelism.Par.Par
import x7c1.colorful.lib.chapter06.State
import x7c1.colorful.lib.chapter08.Gen

import scala.language.higherKinds

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]

  def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

trait Monad[F[_]]extends Functor[F] {
  def unit[A](a: => A): F[A]

  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]

  def map[A,B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A,B,C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  /* 11.3 */

  def sequence[A](lma: List[F[A]]): F[List[A]] = {
    val init: F[List[A]] = unit(List[A]())
    lma.foldRight(init){ (fa, acc) => map2(fa, acc){_ :: _} }
  }

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] = {
    val init: F[List[B]] = unit(List[B]())
    la.foldRight(init){ (a, acc) => map2(f(a), acc){_ :: _} }
  }

  /* 11.4 */

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = {
    sequence(List.fill(n)(ma))
  }

  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] =
    map2(ma, mb)((_, _))


  /* 11.6 */

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = {
    val init: F[List[A]] = unit(List())
    ms.foldRight(init){ (a, acc) =>
      flatMap(f(a)){
        case true => map2(unit(a), acc){_ :: _}
        case _ => acc
      }
    }
  }

  /* 11.7 */

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] = {
    a =>
      // compose9[A,B,C](f)(a)(g)
      flatMap(f(a))(g)
  }

  /* before 11.8 */

  def compose2[A,B,C]:  (A => F[B]) => (B => F[C]) => (A => F[C]) = {
    f => g => a => (flatMap[B, C] _ compose f) apply a apply g
  }
  def compose3[A,B,C]: (A => F[B]) => (B => F[C]) => A => F[C] = {
    (f: A => F[B]) => (g: B => F[C]) => (a: A) =>
      (flatMap[B, C] _ compose f apply a) apply g
  }
  // exchange arguments order (a with g)
  def compose4[A,B,C]: (A => F[B]) => A => (B => F[C]) => F[C] = {
    (f: A => F[B]) => (a: A) => (g: B => F[C]) =>
      (flatMap[B, C] _ compose f apply a) apply g
  }
  def compose5[A,B,C]: (A => F[B]) => A => (B => F[C]) => F[C] = {
    (f: A => F[B]) => (a: A) =>
      flatMap[B, C] _ compose f apply a
  }
  def compose6[A,B,C]: (A => F[B]) => A => (B => F[C]) => F[C] = {
    (f: A => F[B]) =>
      flatMap[B, C] _ compose f
  }
  def compose7[A,B,C]: (A => F[B]) => A => (B => F[C]) => F[C] = {
    (f: A => F[B]) =>
      (flatMap[B, C] _).compose[A] _ apply f
  }
  def compose8[A,B,C]: (A => F[B]) => A => (B => F[C]) => F[C] = {
    (flatMap[B, C] _).compose[A]
  }
  def compose9[A,B,C]: (A => F[B]) => (A => (B => F[C]) => F[C]) = {
    (flatMap[B, C] _).compose[A]
  }

  /*
  class Sample[A, B]{
    def original: A => B = ???
    def composed[C]: (C => A) => (C => B) = original.compose[C]
  }
  */

  /* 11.8 */

  def flatMap_byCompose[A, B](ma: F[A])(f: A => F[B]): F[B] = {
    def revertComposing[X, Y](c: X)
      (f: (X => F[A]) => (X => Y)): F[A] => Y = {
      x => f(_ => x)(c)
    }
    def h[X]: (X => F[A]) => X => (A => F[B]) => F[B] = {
      f => x => g => compose(f, g)(x)
    }
    revertComposing(123)(h)(ma)(f)
  }

  def flatMap_byCompose_2[A, B](ma: F[A])(f: A => F[B]): F[B] = {
    def revertComposing[X](c: X)
      (f: (X => F[A]) => (A => F[B]) => (X => F[B])): F[A] => (A => F[B]) => F[B] = {
      fb => g => f(_ => fb)(g)(c)
    }
     def h[X]: (X => F[A]) => (A => F[B]) => X => F[B] = {
       f => g => compose(f, g)
     }
     revertComposing(123)(h)(ma)(f)

    // or use currying
    // revertCompose(123)((compose[Int, A, B] _).curried)(ma)(f)
  }

  def flatMap_byCompose_3[A, B](ma: F[A])(f: A => F[B]): F[B] = {
    def revertComposing[X](c: X): F[A] => (A => F[B]) => F[B] = {
      fb => g => compose[X, A, B](_ => fb, g)(c)
    }
    revertComposing(123)(ma)(f)
  }

  def flatMap_byCompose_4[A, B](ma: F[A])(f: A => F[B]): F[B] = {
    compose((_: Int) => ma, f)(123)
  }

}

object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    def flatMap[A,B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }
}

object Exercise_11_1 {
  val parMonad = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.unit(a)
    override def flatMap[A, B](ma: Par[A])(f: (A) => Par[B]): Par[B] =
      Par.flatMap(ma)(f)
  }
  val optionMonad = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)
    override def flatMap[A, B](ma: Option[A])(f: (A) => Option[B]): Option[B] =
      ma.flatMap(f)
  }
  val listMonad = new Monad[List]{
    override def unit[A](a: => A): List[A] = List(a)
    override def flatMap[A, B](ma: List[A])(f: (A) => List[B]): List[B] =
      ma.flatMap(f)
  }

  /*
    // following instances are same as above
    val parserMonad = ...
    val streamMonad = ...
   */
}

object Exercise_11_2 {

  def stateMonad[S] = new Monad[({ type X[B] = State[S, B] })#X]{
    override def unit[A](a: => A): State[S, A] =
      State.unit(a)

    override def flatMap[A, B](ma: State[S, A])(f: A => State[S, B]): State[S, B] = {
      ma.flatMap(f)
    }
  }

}

object Exercise_11_5 {
  /*
  replicateM(n, fa)
   1. aligns x-values in fa (A1, A2, ..., Ax)
   2. and aligns them n times (B1, B2, ..., Bn)
   3. then create every patterns (n^x times)
  like below:
    e.g. replicateM(3, Some(1))
      x = 1, n = 3
      => Some(List(1,1,1))

    e.g. replicateM(3, List(1, 2))
      x = 2, n = 3
      => List(List(1,1,1), List(1,1,2), List(1,2,1), L(1,2,2), ...)
   */
}

object Exercise_11_10 {
  /*
    compose(f, unit) == f
      --> compose(f, unit)(n) == f(n)
      --> { a => flatMap(f(a))(unit) }(n) == f(n)
      --> flatMap(f(n))(unit) == f(n)
      --> flatMap(x)(unit) == x

    compose(unit, f) == f
      --> compose(unit, f)(y) == f(y)
      --> { a => flatMap(unit(a))(f) }(y) == f(y)
      --> flatMap(unit(y))(f) == f(y)
   */
}

object Exercise_11_11 {
  /*
    compose(f, unit) == f
      --> compose(f, List(_))(n) == f(n)
      --> { a => flatMap(f(a))(List(_)) }(n) == f(n)
      --> flatMap(f(n))(List(_)) == f(n)
      --> flatMap(f(n))(List(_)) == f(n)
      --> f(n) == f(n)

    compose(unit, f) == f
      --> compose(List(_), f)(y) == f(y)
      --> { a => flatMap(List(a))(f) }(y) == f(y)
      --> flatMap(List(y))(f) == f(y)
      --> f(y) == f(y)
 */
}
