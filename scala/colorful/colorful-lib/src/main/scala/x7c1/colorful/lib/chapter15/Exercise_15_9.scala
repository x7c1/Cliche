package x7c1.colorful.lib.chapter15

import java.io.{File, PrintWriter}

import x7c1.colorful.lib.chapter11.Monad
import x7c1.colorful.lib.chapter13.Exercise_13_2.runTrampoline
import x7c1.colorful.lib.chapter13.Exercise_13_4.translate
import x7c1.colorful.lib.chapter13.Listing_13_15.{runFree, ~>}
import x7c1.colorful.lib.chapter13.{Return, Suspend, Free, Listing_13_9}

import scala.annotation.tailrec

object Exercise_15_9 {

  /*
  draft:

  val process =
    filter(line -> ! line.startsWith("#")) |>
    filter(line -> ! line.isEmpty) |>
    lift(_.toDouble) |>
    lift(toCelsius)

  val free = for {
    f <- openFile("fahrenheit.txt")
    line <- convert(f).by(process)
    _ <- write(line).to("celsius.txt")
  } yield ()

  val free = for {
    celsius <- read.file("fahrenheit.txt").via(process)
    _ <- append.string(celsius).to("celsius.txt")
  } yield ()

  val before = MockFile(
    path = "foo/bar.txt",
    body = """
      #comment1
      140.0
      150.0

      #comment2
      158.0
      160.0
    """
  )
  val (_, after) = mockRunnerOf(free).run(before)
  after.body shouldBe ...
   */

  import Listing_13_9.IO
  type Converter = Process[String, String]

  def onIO(r: File, w: File, process: Converter): IO[Unit] = IO {
    @tailrec
    def go(lines: Iterator[String], p: Converter, writer: PrintWriter): Unit =
      p match {
        case Emit(head, tail) =>
          writer.println(head)
          go(lines, tail, writer)
        case Await(receive) =>
          val next = if (lines.hasNext) receive(Some(lines.next())) else receive(None)
          go(lines, next, writer)
        case Halt() => ()
      }

    val source = io.Source.fromFile(r)
    val writer = new PrintWriter(w)

    try go(source.getLines(), process, writer)
    finally {
      source.close()
      writer.close()
    }
  }

  // read lines as iterator, then write these lines
  def onFree(process: Converter): Free[FileOperation, Unit] = {

    // is there a solution to satisfy @tailrec?
    def write(lines: Iterator[String], p: Converter, handle: HandleW): Free[FileOperation, Unit] =
      p match {
        case Emit(head, tail) =>
          Suspend(WriteLine(handle, head)) flatMap {_ => write(lines, tail, handle)}
        case Await(receive) =>
          val next = if (lines.hasNext) receive(Some(lines.next())) else receive(None)
          write(lines, next, handle)
        case Halt() =>
          Return(())
      }

    for {
      r <- Suspend(OpenToRead("fahrenheit.txt"))
      w <- Suspend(OpenToWrite("celsius.txt"))
      lines <- Suspend(ReadLines(r))
      _ <- write(lines, process, w)
      _ <- Suspend(CloseFile(w))
      _ <- Suspend(CloseFile(r))
    } yield ()
  }

  // read and write a line in order to the end
  def onFree2(process: Converter): Free[FileOperation, Unit] = {

    // is there a solution to satisfy @tailrec?
    def loop(p: Converter, r: HandleR, w: HandleW): Free[FileOperation, Unit] =
      p match {
        case Emit(head, tail) =>
          Suspend(WriteLine(w, head)) flatMap {_ => loop(tail, r, w)}
        case Await(receive) =>
          Suspend(ReadLine(r)) flatMap { line => loop(receive(line), r, w) }
        case Halt() =>
          Return(())
      }

    for {
      r <- Suspend(OpenToRead("fahrenheit.txt"))
      w <- Suspend(OpenToWrite("celsius.txt"))
      _ <- loop(process, r, w)
      _ <- Suspend(CloseFile(w))
      _ <- Suspend(CloseFile(r))
    } yield ()
  }

  def toCelsius(fahrenheit: Double): Double = (5.0 / 9.0) * (fahrenheit - 32.0)

  import Process.{filter, lift}

  val process =
    filter[String](line => ! line.startsWith("#")) |>
    filter(line => ! line.isEmpty) |>
    lift(_.toDouble) |>
    lift(toCelsius) |>
    lift(_.toString)

  type MockRunner = MockBuffer => MockBuffer

  def runBy(f: Converter => Free[FileOperation, Unit]): MockRunner = buffer => {
    val state = MockInterpreter run f(process)
    val (_, after) = state run buffer
    after
  }

  def runToCelsius: MockRunner = runBy(onFree)

  def runToCelsius2: MockRunner = runBy(onFree2)

  def main(args: Array[String]): Unit = {
    val before = MockBuffer(Seq("140.0", "#comment", "149.0"))
    val after = runToCelsius(before)
    println(after)
  }
}

sealed trait FileOperation[A]

case class OpenToRead(file: String) extends FileOperation[HandleR]

case class OpenToWrite(file: String) extends FileOperation[HandleW]

case class ReadLines(h: HandleR) extends FileOperation[Iterator[String]]

case class ReadLine(h: HandleR) extends FileOperation[Option[String]]

case class WriteLine(h: HandleW, line: String) extends FileOperation[Unit]

case class CloseFile(h: Handle) extends FileOperation[Unit]

trait Handle

trait HandleR extends Handle

trait HandleW extends Handle

object ActualInterpreter {
  def run[A](free: Free[FileOperation, A]): A = {
    def converter = new (FileOperation ~> Function0){
      override def apply[X](f: FileOperation[X]): () => X = toThunk(f)
    }
    val f: Free[Function0, A] = translate(free)(converter)
    runTrampoline(f)
  }
  def toThunk[A](f: FileOperation[A]): () => A = ???
}

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
