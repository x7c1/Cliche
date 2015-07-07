package x7c1.colorful.lib.chapter15

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

}

object Process
  extends Exercise_15_1
  with Exercise_15_2
{
  /* Listing 15-5 */

  def liftOne[I,O](f: I => O): Process[I,O] = Await {
    case Some(i) => Emit(f(i))
    case None => Halt()
  }

  /* Listing 15-7 */

  def lift[I,O](f: I => O): Process[I,O] = liftOne(f).repeat
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
    def go[I2,O2](n: Int): Process[I2, Int] = Await {
      case Some(_) => Emit(n, go(n + 1))
      case _ => Halt()
    }
    go(1)
  }
}
