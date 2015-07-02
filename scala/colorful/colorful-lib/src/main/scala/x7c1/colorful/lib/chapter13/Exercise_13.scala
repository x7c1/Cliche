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


}





  }
}
