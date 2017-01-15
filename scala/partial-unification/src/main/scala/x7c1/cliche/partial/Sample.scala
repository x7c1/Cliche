package x7c1.cliche.partial

import scala.language.{higherKinds, reflectiveCalls}

trait Monad[F[_]] {

  def unit[A](x: => A): F[A]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
}

trait CanConvert[A, F0[_], F[_]] {
  def from(x: F0[A]): F[A]
}

object CanConvert {

  implicit def endoImpl[A, F[_]]: CanConvert[A, F, F] = identity(_)

}

class Impl[A, F[_] : Monad](x: F[A]) {

  def map[B](f: A => B): F[B] = {
    val m = implicitly[Monad[F]]
    m.flatMap(x)(f andThen (m unit _))
  }

  def flatMap[F0[_], B](f: A => F0[B])(implicit convert: CanConvert[B, F0, F]): F[B] = {
    val m = implicitly[Monad[F]]
    m.flatMap(x)(f andThen convert.from)
  }
}

trait Option2[A]

object Option2 {

  implicit object monad extends Monad[Option2] {
    override def unit[A](x: => A): Option2[A] = ???

    override def flatMap[A, B](x: Option2[A])(f: (A) => Option2[B]): Option2[B] = ???
  }

  implicit class MonadLike[A](x: Option2[A]) extends Impl[A, Option2](x)

}

trait Either2[L, R]

object Either2 {

  /*
  implicit def toOption2[A, L]: CanConvert[A, Either2[L, ?], Option2] =
    new CanConvert[A, Either2[L, ?], Option2] {
      override def from(x: Either2[L, A]): Option2[A] = ???
    }
  */

  // same as above
  implicit def toOption2[A, L]: CanConvert[A, Either2[L, ?], Option2] = {
    (x: Either2[L, A]) => ???
  }

  implicit def monad[L, R]: Monad[Either2[L, ?]] = ???

  implicit class MonadLike[L, R](x: Either2[L, R]) extends Impl[R, Either2[L, ?]](x)

}

class Sample {

  def option: Option2[Int] = ???

  def either: Either2[Exception, Int] = ???

  def either1 =
    for {
      n1 <- either
      n2 <- either
    } yield {
      n1 + n2
    }

  def option1 =
    for {
      n1 <- option
      n2 <- either
    } yield {
      n1 + n2
    }

}
