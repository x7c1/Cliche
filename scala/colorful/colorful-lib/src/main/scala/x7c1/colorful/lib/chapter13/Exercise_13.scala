package x7c1.colorful.lib.chapter13

import fpinscala.parallelism.Par
import fpinscala.parallelism.Par.Par
import x7c1.colorful.lib.chapter11.Monad
import x7c1.colorful.lib.chapter13.Listing_13_21.ConsoleReader

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

  def map[B](f: A => B): Free[F,B] = flatMap(f andThen (Return(_)))
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

object Listing_13_15 {
  import scala.io.StdIn.readLine

  sealed trait Console[A] {
    def toPar: Par[A]

    def toThunk: () => A

    import Listing_13_21.ConsoleReader
    def toReader: ConsoleReader[A]
  }

  case object ReadLine extends Console[Option[String]] {
    def toPar = Par.lazyUnit(run)

    def toThunk = () => run

    def run: Option[String] = {
      try Some(readLine())
      catch { case e: Exception => None }
    }
    override def toReader: ConsoleReader[Option[String]] = ConsoleReader(Some(_))
  }

  case class PrintLine(line: String) extends Console[Unit] {
    def toPar = Par.lazyUnit(println(line))
    def toThunk = () => println(line)
    override def toReader: ConsoleReader[Unit] = ConsoleReader{_ => ()}
  }

  /* Listing 13-16 */

  object Console {
    type ConsoleIO[A] = Free[Console, A]
    def readLn: ConsoleIO[Option[String]] = Suspend(ReadLine)
    def printLn(line: String): ConsoleIO[Unit] = Suspend(PrintLine(line))
  }

  /* Listing 13-17 */

  trait Translate[F[_], G[_]] {
    def apply[A](f: F[A]): G[A]
  }
  type ~>[F[_], G[_]] = Translate[F,G]

  val consoleToFunction0 = new (Console ~> Function0) {
    def apply[A](a: Console[A]) = a.toThunk
  }
  val consoleToPar = new (Console ~> Par) {
    def apply[A](a: Console[A]) = a.toPar
  }

  /* Listing 13-18 */

  import Exercise_13_3.step

  def runFree[F[_],G[_],A](free: Free[F,A])(t: F ~> G)(implicit G: Monad[G]): G[A] =
    step(free) match {
      case Return(a) => G.unit(a)
      case Suspend(r) => t(r)
      case FlatMap(Suspend(r), f) => G.flatMap(t(r))(a => runFree(f(a))(t))
      case _ => sys.error("Impossible; `step` eliminates these cases")
    }

  /* Listing 13-19 */

  def runConsoleFunction0[A](a: Free[Console,A]): () => A =
    runFree[Console,Function0,A](a)(consoleToFunction0)

  def runConsolePar[A](a: Free[Console,A]): Par[A] =
    runFree[Console,Par,A](a)(consoleToPar)

  /* Listing 13-20 */

  implicit val function0Monad = new Monad[Function0] {
    def unit[A](a: => A) = () => a
    def flatMap[A,B](a: Function0[A])(f: A => Function0[B]) = () => f(a())()
  }
  implicit val parMonad = new Monad[Par] {
    def unit[A](a: => A) = Par.unit(a)
    def flatMap[A,B](a: Par[A])(f: A => Par[B]) = Par.fork { Par.flatMap(a)(f) }
  }

  def main(args: Array[String]) {
    import Console.{printLn, readLn}

    val f1: Free[Console, Option[String]] = for {
      _ <- printLn("I can only interact with the console.")
      ln <- readLn
    } yield ln

    val y1 = runConsoleFunction0(f1)()
    println(y1)
  }
}

object Exercise_13_4 {
  import x7c1.colorful.lib.chapter13.Listing_13_15.{~>, Console, runFree, consoleToFunction0}
  import Exercise_13_1.freeMonad
  import Exercise_13_2.runTrampoline

  def translate[F[_],G[_],A](f: Free[F,A])(fg: F ~> G): Free[G,A] = {
    type FreeG[X] = Free[G, X]
    val M: Monad[FreeG] = freeMonad[G]

    val p = new (F ~> FreeG){
      override def apply[Y](f: F[Y]): Free[G, Y] = {
        val gy: G[Y] = fg(f)
        val fgy: Free[G,Y] = Suspend(gy)
        fgy
      }
    }
    runFree(f)(p)(M)
  }

  def runConsole[A](a: Free[Console,A]): A = {
    def f: Console ~> Function0 = consoleToFunction0
    val free: Free[Function0, A] = translate(a)(f)

    runTrampoline(free)
  }
}

object Listing_13_21 {
  import x7c1.colorful.lib.chapter13.Listing_13_15.{~>, Console, runFree}
  import Console.ConsoleIO

  case class ConsoleReader[A](run: String => A) {
    def map[B](f: A => B): ConsoleReader[B] =
      ConsoleReader(r => f(run(r)))

    def flatMap[B](f: A => ConsoleReader[B]): ConsoleReader[B] =
      ConsoleReader(r => f(run(r)).run(r))
  }
  object ConsoleReader {
    implicit val monad = new Monad[ConsoleReader] {
      def unit[A](a: => A) = ConsoleReader(_ => a)
      def flatMap[A, B](ra: ConsoleReader[A])(f: A => ConsoleReader[B]) = ra flatMap f
    }
  }

  /* Listing 13-22 */

  val consoleToReader = new (Console ~> ConsoleReader) {
    def apply[A](a: Console[A]) = a.toReader
  }
  def runConsoleReader[A](io: ConsoleIO[A]): ConsoleReader[A] =
    runFree[Console,ConsoleReader,A](io)(consoleToReader)
}
