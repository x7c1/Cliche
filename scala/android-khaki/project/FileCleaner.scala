import sbt.{File, Logger}

object FileCleaner {

  def remove(file: File): Unit = {
    sbt.Defaults.doClean(
      clean = Seq(file),
      preserve = Seq()
    )
  }

  object withLogging {
    def remove(file: File): Reader[Logger, Unit] = Reader { logger =>
      FileCleaner remove file
      logger info s"[done] removed: $file"
    }
  }

}
