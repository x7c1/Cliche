package x7c1.colorful.lib.chapter15

import x7c1.colorful.lib.chapter13.Exercise_13_2.runTrampoline
import x7c1.colorful.lib.chapter13.Exercise_13_4.translate
import x7c1.colorful.lib.chapter13.Free
import x7c1.colorful.lib.chapter13.Listing_13_15.~>

object Exercise_15_9 {

  val factory = ConverterFactory(
    fahrenheitFile = "fahrenheit.txt",
    celsiusFile = "celsius.txt"
  )

  def runMockViaIterator: MockBuffer => MockBuffer = MockRunner by factory.createViaIterator

  def runMock: MockBuffer => MockBuffer = MockRunner by factory.create

  def main(args: Array[String]): Unit = {
    val before = MockBuffer(Seq("140.0", "#comment", "149.0"))
    val after = runMockViaIterator(before)
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
