package x7c1.colorful.lib.chapter15

import x7c1.colorful.lib.chapter13.Listing_13_6.fahrenheitToCelsius
import x7c1.colorful.lib.chapter13.{Free, Suspend}

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

  def loop(r: HandleR, w: HandleW): Free[FileOperation, Unit] = for {
    line <- Suspend(ReadLine(r))
    _ <- {
      val f: FileOperation[Unit] = line match {
        case Some(s) =>  WriteLine(w, fahrenheitToCelsius(s.toDouble).toString)
        case None => IgnoreLine
      }
      Suspend(f) flatMap {_ => loop(r, w)}
    }
  } yield ()

  def convertFile = for {
    f <- Suspend(OpenToRead("fahrenheit.txt"))
    c <- Suspend(OpenToWrite("celsius.txt"))
    _ <- loop(f, c)
  } yield ()
}

trait FileOperation[A]

case class OpenToRead(file: String) extends FileOperation[HandleR]

case class OpenToWrite(file: String) extends FileOperation[HandleW]

case class ReadLine(h: HandleR) extends FileOperation[Option[String]]

case class WriteLine(h: HandleW, line: String) extends FileOperation[Unit]

case object IgnoreLine extends FileOperation[Unit]

trait HandleR

trait HandleW
