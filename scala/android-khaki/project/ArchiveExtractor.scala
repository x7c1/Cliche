import sbt.Process.stringToProcess
import sbt.{File, ProcessLogger}

object ArchiveExtractor {
  def apply(logger: ProcessLogger, destination: File): ArchiveExtractor = {
    new ArchiveExtractor(logger, destination)
  }
}

class ArchiveExtractor private(logger: ProcessLogger, destination: File) {

  def unzip(archive: File): Int = {
    s"unzip -o -d ${destination.getAbsolutePath} ${archive.getAbsolutePath}" !< logger
  }
}
