package x7c1.colorful.lib.chapter15.former

import scala.language.higherKinds

sealed trait Process[I,O]{

  /* Listing 15-4 */

  def apply(s: Stream[I]): Stream[O] = this match {
    case Halt() => Stream()
    case Await(recv) => s match {
      case h #:: t => recv(Some(h))(t)
      case xs => recv(None)(xs)
    }
    case Emit(h,t) => h #:: t(s)
  }

  /* Listing 15-6 */

  def repeat: Process[I,O] = {
    def go(p: Process[I,O]): Process[I,O] = p match {
      case Halt() => go(this)
      case Await(recv) => Await {
        case None => recv(None)
        case i => go(recv(i))
      }
      case Emit(h, t) => Emit(h, go(t))
    }
    go(this)
  }

  /* Exercise 15.5 */

  def |> [O2](p2: Process[O,O2]): Process[I,O2] = p2 match {
    case Emit(h, t) => Emit(h, this |> t)
    case Halt() => Halt()
    case Await(recv) => this match {
      case Emit(head, tail) => tail |> recv(Option(head))
      case Await(f) => Await(i => f(i) |> p2)
      case Halt() => Halt() |> recv(None)
    }
  }

  /* Listing 15-10 */

  def ++ (p: => Process[I,O]): Process[I,O] = this match {
    case Halt() => p
    case Emit(h, t) => Emit(h, t ++ p)
    case Await(recv) => Await(recv andThen (_ ++ p))
  }

  /* Listing 15-11 */

  def flatMap[O2](f: O => Process[I,O2]): Process[I,O2] = this match {
    case Halt() => Halt()
    case Emit(h, t) => f(h) ++ t.flatMap(f)
    case Await(recv) => Await(recv andThen (_ flatMap f))
  }

  /* Exercise 15.6 */

  def zipWithIndex: Process[I, (O,Int)] = {
    def go(n: Int, p: Process[I,O]): Process[I,(O,Int)] = p match {
      case Halt() => Halt()
      case Emit(h, t) => Emit((h, n), go(n + 1, t))
      case Await(recv) => Await { x => go(n, recv(x)) }
    }
    go(0, this)
  }

  def map[O2](f: O => O2): Process[I,O2] = this |> Process.lift(f)
}

object Process
  extends Exercise_15_1
  with Exercise_15_2
  with Exercise_15_3
  with Exercise_15_4
  with Exercise_15_7
  with Exercise_15_8
{
  /* Listing 15-5 */

  def liftOne[I,O](f: I => O): Process[I,O] = Await {
    case Some(i) => Emit(f(i))
    case None => Halt()
  }

  /* Listing 15-7 */

  def lift[I,O](f: I => O): Process[I,O] = liftOne(f).repeat

  /* Listing 15-8 */

  def filter[I](p: I => Boolean): Process[I,I] = Await[I,I] {
    case Some(i) if p(i) => emit(i)
    case _ => Halt()
  }.repeat

  /* Listing 15-9 */

  def loop[S,I,O](z: S)(f: (I,S) => (O,S)): Process[I,O] = await(
    (i: I) => f(i,z) match {
      case (o,s2) => emit(o, loop(s2)(f))
    }
  )

  def await[I,O](f: I => Process[I,O],
    fallback: Process[I,O] = Halt[I,O]()): Process[I,O] = {

    Await[I,O] {
      case Some(i) => f(i)
      case None => fallback
    }
  }

  def emit[I,O](
    head: O,
    tail: Process[I,O] = Halt[I,O]()): Process[I,O] = {

    Emit(head, tail)
  }
}

case class Emit[I,O](
  head: O,
  tail: Process[I,O] = Halt[I,O]()) extends Process[I,O]

case class Await[I,O](
  recv: Option[I] => Process[I,O]) extends Process[I,O]

case class Halt[I,O]() extends Process[I,O]

trait Exercise_15_1 {
  self: Process.type =>

  def take[I](n: Int): Process[I,I] = Await {
    case Some(i) if n > 0 => Emit(i, take(n - 1))
    case _ => Halt()
  }

  def drop[I](n: Int): Process[I,I] = Await {
    case Some(i) if n > 0 => drop(n - 1)
    case Some(i) => Emit(i, lift[I,I](i => i))
    case _ => Halt()
  }

  def takeWhile[I](f: I => Boolean): Process[I,I] = Await {
    case Some(i) if f(i) => Emit(i, takeWhile(f))
    case _ => Halt()
  }

  def dropWhile[I](f: I => Boolean): Process[I,I] = Await {
    case Some(i) if f(i) => dropWhile(f)
    case Some(i) => Emit(i, lift[I,I](i => i))
    case _ => Halt()
  }
}

trait Exercise_15_2 {
  self: Process.type =>

  def count[I]: Process[I,Int] = {
    def go(n: Int): Process[I, Int] = Await {
      case Some(_) => Emit(n, go(n + 1))
      case _ => Halt()
    }
    go(1)
  }
}

trait Exercise_15_3 {
  self: Process.type =>

  def mean: Process[Double,Double] = {
    def go(sum: Double, n: Int): Process[Double, Double] = Await {
      case Some(d) => Emit((sum + d) / n, go(sum + d, n + 1))
      case _ => Halt()
    }
    go(0.0, 1)
  }
}

trait Exercise_15_4 {
  self: Process.type =>

  def sum2: Process[Double,Double] = loop(0.0){ (d, acc) =>
    (d + acc) -> (d + acc)
  }
  def count2[I]: Process[I,Int] = loop(0){ (i, acc) =>
    (acc + 1, acc + 1)
  }
}

trait Exercise_15_7 {
  self: Process.type =>

  def mean2: Process[Double,Double] = {
    val x = zip(count2[Double], sum2)
    x map { case (i, d) => d / i }
  }

  /* copied from answer */

  def zip[A,B,C](p1: Process[A,B], p2: Process[A,C]): Process[A,(B,C)] =
    (p1, p2) match {
      case (Halt(), _) => Halt()
      case (_, Halt()) => Halt()
      case (Emit(b, t1), Emit(c, t2)) => Emit((b,c), zip(t1, t2))
      case (Await(recv1), _) =>
        Await((oa: Option[A]) => zip(recv1(oa), feed(oa)(p2)))
      case (_, Await(recv2)) =>
        Await((oa: Option[A]) => zip(feed(oa)(p1), recv2(oa)))
    }

  def feed[A,B](oa: Option[A])(p: Process[A,B]): Process[A,B] =
    p match {
      case Halt() => p
      case Emit(h,t) => Emit(h, feed(oa)(t))
      case Await(recv) => recv(oa)
    }
}

trait Exercise_15_8 {
  self: Process.type =>

  def exists[I](f: I => Boolean): Process[I,Boolean] =
    filter(f) |> Await {
      case Some(i) => emit(true)
      case None => emit(false)
    }
}

object Exercise_15_9 {
  val factory = ConverterFactory(
    fahrenheitFile = "fahrenheit.txt",
    celsiusFile = "celsius.txt"
  )
  def runMockViaIterator: MockBuffer => MockBuffer = {
    MockRunner by factory.createViaIterator
  }
  def runMock: MockBuffer => MockBuffer = {
    MockRunner by factory.create
  }
  def main(args: Array[String]): Unit = {
    val before = MockBuffer(Seq("140.0", "#comment", "149.0"))
    val after = runMockViaIterator(before)
    println(after)
  }
}
