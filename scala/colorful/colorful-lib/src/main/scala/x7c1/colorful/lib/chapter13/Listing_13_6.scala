package x7c1.colorful.lib.chapter13

import x7c1.colorful.lib.chapter11.Monad
import scala.io.StdIn.readLine


object Listing_13_6 {
  sealed trait IO[A] { self =>
    def run: A

    def map[B](f: A => B): IO[B] =
      new IO[B] { def run = f(self.run) }

    def flatMap[B](f: A => IO[B]): IO[B] =
      new IO[B] { def run = f(self.run).run }
  }

  object IO extends Monad[IO] {
    def unit[A](a: => A): IO[A] = new IO[A] { def run = a }

    def flatMap[A,B](fa: IO[A])(f: A => IO[B]) = fa flatMap f

    def apply[A](a: => A): IO[A] = unit(a)
  }

  def fahrenheitToCelsius(f: Double): Double = (f - 32) * 5.0/9.0

  def ReadLine: IO[String] = IO { readLine() }

  def PrintLine(msg: String): IO[Unit] = IO { println(msg) }

  def converter: IO[Unit] = for {
    _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
    d <- ReadLine.map(_.toDouble)
    _ <- PrintLine(fahrenheitToCelsius(d).toString)
  } yield ()

  def main(args: Array[String]) {
    converter.run
  }
}
