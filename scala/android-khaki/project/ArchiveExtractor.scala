import sbt.Process.stringToProcess
import sbt.{File, Logger}

object ArchiveExtractor {
  def apply(logger: Logger, destination: File): ArchiveExtractor = {
    new ArchiveExtractor(logger, destination)
  }
}

class ArchiveExtractor private(logger: Logger, destination: File) {

  def unzip(archive: File): Int = {
    s"unzip -o -d ${destination.getAbsolutePath} ${archive.getAbsolutePath}" !< logger
  }
}
