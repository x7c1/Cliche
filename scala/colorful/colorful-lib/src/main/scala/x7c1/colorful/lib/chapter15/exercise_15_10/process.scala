package x7c1.colorful.lib.chapter15.exercise_15_10

import java.util.concurrent.ExecutorService

import fpinscala.parallelism.Par
import x7c1.colorful.lib.chapter11.{Monad, Exercise_11_1}
import x7c1.colorful.lib.chapter13.{Exercise_13_3, Listing_13_9}

import scala.language.higherKinds

trait Process[F[_],O]{
  import x7c1.colorful.lib.chapter15.exercise_15_10.Process.{Await, Halt, Emit, Try, End, Kill, await}

  /* Listing 15-15 */
  def onHalt(f: Throwable => Process[F,O]): Process[F,O] = this match {
    case Halt(e) => Try(f(e))
    case Emit(h, t) => Emit(h, t.onHalt(f))
    case Await(req,recv) => Await(req, recv andThen (_.onHalt(f)))
  }
  def ++(p: => Process[F,O]): Process[F,O] =
    this.onHalt {
      case End => Try(p)
      case err => Halt(err)
    }

  /* Listing 15-18 */
  def flatMap[O2](f: O => Process[F,O2]): Process[F,O2] =
    this match {
      case Halt(err) => Halt(err)
      case Emit(o, t) => Try(f(o)) ++ t.flatMap(f)
      case Await(req, recv) => Await(req, recv andThen (_ flatMap f))
    }

  /* Exercise 15.10 */
  def runLog(implicit F: MonadCatch[F]): F[IndexedSeq[O]] = {
    // from answer
    def go(p: Process[F,O], acc: IndexedSeq[O]): F[IndexedSeq[O]] = {
      p match {
        case Await(req, recv) =>  F.flatMap(F.attempt(req)){e => go(Try(recv(e)), acc)}
        case Halt(End) => F.unit(acc)

        case Halt(err) => F.fail(err)
        case Emit(head, tail) => go(tail, acc :+ head)
      }
    }
    go(this, IndexedSeq())
  }

  /* Listing 15-20 */
  def onComplete(p: => Process[F,O]): Process[F,O] = this.onHalt {
    case End => p.asFinalizer
    case err => p.asFinalizer ++ Halt(err)
  }

  /* Listing 15-21 */
  def asFinalizer: Process[F,O] = this match {
    case Emit(h, t) => Emit(h, t.asFinalizer)
    case Halt(e) => Halt(e)
    case Await(req,recv) => await(req) {
      case Left(Kill) => this.asFinalizer
      case x => recv(x)
    }
  }
}

object Process {

  case class Await[F[_],A,O](
    req: F[A],
    recv: Either[Throwable, A] => Process[F,O]) extends Process[F,O]

  case class Emit[F[_],O](
    head: O,
    tail: Process[F,O]) extends Process[F,O]

  case class Halt[F[_],O](err: Throwable) extends Process[F,O]

  case object End extends Exception

  case object Kill extends Exception

  /* Listing 15-16 */
  def Try[F[_],O](p: => Process[F,O]): Process[F,O] =
    try p
    catch { case e: Throwable => Halt(e) }

  /* Listing 15-17 */
  def await[F[_],A,O]
    (req: F[A])
    (recv: Either[Throwable,A] => Process[F,O]): Process[F,O] = {

    Await(req, recv)
  }

  import Listing_13_9.IO

  /* Exercise 15.11 */
  def eval[F[_],A](a: F[A]): Process[F,A] = await[F,A,A](a){
    case Left(x) => Halt(x)
    case Right(x) => Emit(x, Halt(End))
  }
  def eval_[F[_],A,B](a: F[A]): Process[F,B] = {
    // from answer : ignore all output of a Process
    def drain: Process[F,A] => Process[F,B] = {
      case Await(req, recv) => Await(req, recv andThen drain)
      case Emit(head, tail) => drain(tail)
      case Halt(err) => Halt(err)
    }
    drain(eval(a))
  }

  def resource[R,O]
    (acquire: IO[R])
    (use: R => Process[IO,O])
    (release: R => Process[IO,O]): Process[IO,O] = {

    eval(acquire) flatMap { r => use(r).onComplete(release(r)) }
  }

  /* Listing 15-24 */
  type Process1[I,O] = Process[Is[I]#f, O]
}

trait MonadCatch[F[_]] extends Monad[F] {
  def attempt[A](a: F[A]): F[Either[Throwable,A]]
  def fail[A](t: Throwable): F[A]
}

/* Listing 15-23 */
case class Is[I]() {
  sealed trait f[X]
  val Get = new f[I] {}
}

