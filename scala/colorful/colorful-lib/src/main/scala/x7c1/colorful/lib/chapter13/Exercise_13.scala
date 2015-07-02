package x7c1.colorful.lib.chapter13

import x7c1.colorful.lib.chapter11.Monad

import scala.annotation.tailrec
import scala.language.{reflectiveCalls, higherKinds}


trait Listing_13_8 {
  trait IO[A] {
    def flatMap[B](f: A => IO[B]): IO[B] =
      FlatMap(this, f)

    def map[B](f: A => B): IO[B] =
      flatMap(f andThen (Return(_)))
  }

  case class Return[A](a: A) extends IO[A]

  case class Suspend[A](resume: () => A) extends IO[A]

  case class FlatMap[A,B](sub: IO[A], k: A => IO[B]) extends IO[B]

  object IO extends Monad[IO] with MonadProc[IO]{

    override def unit[A](a: => A): IO[A] = new IO[A] { def run = a }

    override def flatMap[A,B](fa: IO[A])(f: A => IO[B]) = fa flatMap f

    def apply[A](a: => A): IO[A] = unit(a)

    /* Listing 13.10 */

    @tailrec
    def run[A](io: IO[A]): A = io match {
      case Return(a) => a
      case Suspend(r) => r()
      case FlatMap(x, f) => x match {
        case Return(a) => run(f(a))
        case Suspend(r) => run(f(r()))
        case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
      }
    }
  }
  trait MonadProc[F[_]] {
    self: Monad[F] =>

    def forever[A,B](a: F[A]): F[B] = {
      lazy val t: F[B] = forever[A, B](a)
      flatMap(a){_ => t}
    }
  }
}

object Listing_13_9 extends Listing_13_8 {
  def main(args: Array[String]) {
    def printLine(s: String): IO[Unit] = Suspend(() => println(s))
    val p = IO.forever(printLine("Still going..."))
    IO run p
  }
}

object Listing_13_11 {

  sealed trait TailRec[A] {
    def flatMap[B](f: A => TailRec[B]): TailRec[B] = FlatMap(this, f)
    def map[B](f: A => B): TailRec[B] =  flatMap(f andThen (Return(_)))
  }
  case class Return[A](a: A) extends TailRec[A]
  case class Suspend[A](resume: () => A) extends TailRec[A]
  case class FlatMap[A,B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]
}

/* Listing 13-14 */

sealed trait Free[F[_],A] {
  def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMap(this, f)
}
case class Return[F[_],A](a: A) extends Free[F,A]

case class Suspend[F[_],A](s: F[A]) extends Free[F,A]

case class FlatMap[F[_],A,B](s: Free[F,A], f: A => Free[F,B]) extends Free[F,B]

object Exercise_13_1 {
  def freeMonad[F[_]]: Monad[({type f[a] = Free[F,a]})#f] =
    new Monad[({type f[a] = Free[F, a]})#f] {
      override def unit[A](a: => A): Free[F, A] = Return(a)
      override def flatMap[A, B](ma: Free[F, A])
        (f: A => Free[F, B]): Free[F, B] = FlatMap(ma, f)
    }
}

object Exercise_13_2 {
  @tailrec
  def runTrampoline[A](fa: Free[Function0,A]): A = fa match {
    case Return(a) => a
    case Suspend(s) => s()
    case FlatMap(s1, f1) => s1 match {
      case Return(a) => runTrampoline(f1(a))
      case Suspend(s) => runTrampoline(f1(s()))
      case FlatMap(y, g) => runTrampoline(y.flatMap{a => g(a) flatMap f1})
    }
  }
}

object Exercise_13_3 {

  @tailrec
  def step[F[_], A](free: Free[F,A]): Free[F,A] = free match {
    case FlatMap(FlatMap(x,f), g) => step(x flatMap (a => f(a) flatMap g))
    case FlatMap(Return(x), f) => step(f(x))
    case _ => free
  }

  def run[F[_],A](fa: Free[F,A])(implicit F: Monad[F]): F[A] = step(fa) match {
    case Return(a) => F.unit(a)
    case Suspend(s) => s
    case FlatMap(s, f) => s match {
      case Suspend(s2) => F.flatMap(s2){a => run(f(a))}
      case _ => sys.error("Impossible; `step` eliminates these cases")
    }
  }
}
