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

  /* Listing 15-5 */

  def liftOne[I,O](f: I => O): Process[I,O] = Await {
    case Some(i) => Emit(f(i))
    case None => Halt()
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

  /* Listing 15-7 */

  def lift[I,O](f: I => O): Process[I,O] = liftOne(f).repeat
}

case class Emit[I,O](
  head: O,
  tail: Process[I,O] = Halt[I,O]()) extends Process[I,O]

case class Await[I,O](
  recv: Option[I] => Process[I,O]) extends Process[I,O]

case class Halt[I,O]() extends Process[I,O]
