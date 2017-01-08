import sbt.{Def, Logger, Task}
import sbt.Def.Initialize

case class Reader[X, A](run: X => A) {
  def map[B](f: A => B): Reader[X, B] = {
    new Reader[X, B](x => f(run(x)))
  }

  def flatMap[B](f: A => Reader[X, B]): Reader[X, B] = {
    new Reader[X, B](x => f(run(x)) run x)
  }
}

object Reader {

  import scala.language.implicitConversions

  implicit class RichUnitReader[A](reader: Reader[A, Unit]) {
    def append(next: Reader[A, Unit]): Reader[A, Unit] = {
      reader.flatMap(_ => next)
    }
  }

  implicit def forLogger[A](reader: Reader[Logger, A]): Initialize[Task[A]] = {
    Def task {
      val logger = sbt.Keys.streams.value.log
      reader run logger
    }
  }

}
