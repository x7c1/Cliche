package x7c1.colorful.lib.chapter15

import x7c1.colorful.lib.chapter11.Monad
import x7c1.colorful.lib.chapter13.Free
import x7c1.colorful.lib.chapter13.Listing_13_15._

case class MockBuffer(
  lines: Seq[String],
  logs: Vector[String] = Vector(),
  closed: Vector[Handle] = Vector()){

  def log(message: String) = copy(logs = logs :+ message)

  def close(handle: Handle) = copy(closed = closed :+ handle)
}

case class MockState[A](run: MockBuffer => (A, MockBuffer))

object MockState {
  implicit val monad: Monad[MockState] = new Monad[MockState]{
    override def unit[A](a: => A): MockState[A] = {
      MockState(buffer => (a, buffer))
    }
    override def flatMap[A, B](ma: MockState[A])(f: A => MockState[B]): MockState[B] =
      MockState[B]{ buffer =>
        val (a, state) = ma run buffer
        f(a) run state
      }
  }
}

case class MockHandlerToWrite(file: String) extends HandleW

case class MockHandlerToRead(file: String) extends HandleR

object MockInterpreter {

  def toMockState = new (FileOperation ~> MockState){
    override def apply[A](f: FileOperation[A]): MockState[A] = f match {
      case OpenToRead(file) => MockState { buffer =>
        MockHandlerToRead(file) -> buffer.log(s"open to read $file")
      }
      case OpenToWrite(file) => MockState { buffer =>
        MockHandlerToWrite(file) -> buffer.log(s"open to write $file")
      }
      case ReadLines(handler) => MockState { buffer =>
        buffer.lines.toIterator -> buffer.log("read lines")
      }
      case ReadLine(handler) => MockState { buffer =>
        val (line, next) = buffer.lines match {
          case head +: tail => Some(head) -> buffer.copy(lines = tail)
          case _ => None -> buffer
        }
        line -> next.log(s"read line $line")
      }
      case WriteLine(handler, line) => MockState { buffer =>
        () -> buffer.log(s"write line $line")
      }
      case CloseFile(handler) => MockState { buffer =>
        () -> buffer.log(s"close file by $handler").close(handler)
      }
    }
  }
  def run[A](free: Free[FileOperation, A]): MockState[A] = {
    runFree(free)(toMockState)
  }
}

object MockRunner {
  type Runner = MockBuffer => MockBuffer

  def by(free: Free[FileOperation, Unit]): MockBuffer => MockBuffer = { buffer =>
    val state = MockInterpreter run free
    val (_, after) = state run buffer
    after
  }
}
