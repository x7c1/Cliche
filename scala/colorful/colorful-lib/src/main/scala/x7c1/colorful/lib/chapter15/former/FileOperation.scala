package x7c1.colorful.lib.chapter15.former

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
