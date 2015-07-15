package x7c1.colorful.lib.chapter15

import java.io.{PrintWriter, File}

import x7c1.colorful.lib.chapter13.{Return, Suspend, Free, Listing_13_9}

import scala.annotation.tailrec

object ConverterFactory {
  import Process.{filter, lift}

  val process =
    filter[String](line => ! line.startsWith("#")) |>
    filter(line => ! line.isEmpty) |>
    lift(_.toDouble) |>
    lift(toCelsius) |>
    lift(_.toString)

  def toCelsius(fahrenheit: Double): Double = (5.0 / 9.0) * (fahrenheit - 32.0)

  def apply(fahrenheitFile: String, celsiusFile: String): ConverterFactory = {
    new ConverterFactory(fahrenheitFile, celsiusFile, process)
  }
}

class ConverterFactory(
  fahrenheitFile: String,
  celsiusFile: String,
  process: Process[String, String]){

  type Converter = Free[FileOperation, Unit]

  // read and write a line in order to the end
  def create: Converter = {

    // is there a solution to satisfy @tailrec?
    def loop(p: Process[String, String], r: HandleR, w: HandleW): Converter =
      p match {
        case Emit(head, tail) =>
          Suspend(WriteLine(w, head)) flatMap {_ => loop(tail, r, w)}
        case Await(receive) =>
          Suspend(ReadLine(r)) flatMap { line => loop(receive(line), r, w) }
        case Halt() =>
          Return(())
      }

    for {
      r <- Suspend(OpenToRead(fahrenheitFile))
      w <- Suspend(OpenToWrite(celsiusFile))
      _ <- loop(process, r, w)
      _ <- Suspend(CloseFile(w))
      _ <- Suspend(CloseFile(r))
    } yield ()
  }

  // read lines as iterator, then write these lines
  def createViaIterator: Converter = {

    // is there a solution to satisfy @tailrec?
    def write(lines: Iterator[String], p: Process[String, String], handle: HandleW): Converter =
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
      r <- Suspend(OpenToRead(fahrenheitFile))
      w <- Suspend(OpenToWrite(celsiusFile))
      lines <- Suspend(ReadLines(r))
      _ <- write(lines, process, w)
      _ <- Suspend(CloseFile(w))
      _ <- Suspend(CloseFile(r))
    } yield ()
  }

  import Listing_13_9.IO
  def onIO(r: File, w: File, process: Process[String, String]): IO[Unit] = IO {
    @tailrec
    def go(lines: Iterator[String], p: Process[String, String], writer: PrintWriter): Unit =
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
}
