import sbt.Def.Classpath
import sbt.Path.richFile
import sbt.{Attributed, File, ModuleID, PathFinder, ProcessLogger}


sealed trait ArchiveCacheSplicer {
  def run(logger: ProcessLogger): Unit

  def loadClasspath: Classpath
}

object ArchiveCacheSplicer {

  implicit class RichSplicers(splicers: Seq[ArchiveCacheSplicer]) {
    def runAll(logger: ProcessLogger) = {
      splicers foreach (_ run logger)
    }

    def loadAllClasspath: Classpath = {
      val empty: Classpath = Attributed blankSeq Seq()
      splicers.foldLeft(empty)(_ ++ _.loadClasspath)
    }
  }

  class Factory(
    project: File,
    cacheDirectory: File,
    unmanagedDirectory: File) {

    def create(moduleId: ModuleID): ArchiveCacheSplicer = {
      new ArchiveCacheFinder(cacheDirectory) find moduleId match {
        case Some(cache: AarCache) =>
          new AarCacheExpander(unmanagedDirectory, cache)
        case Some(cache) =>
          throw new IllegalArgumentException(s"unknown archive type: ${cache.getClass.getName}")
        case None =>
          throw new IllegalArgumentException(s"module not found: $moduleId")
      }
    }

  }

}

class AarCacheExpander(
  unmanagedDirectory: File,
  cache: AarCache) extends ArchiveCacheSplicer {

  private val destination = {
    unmanagedDirectory / cache.moduleId.name
  }

  override def loadClasspath = {
    val dirs = Seq(
      destination / "classes.jar",
      destination / "libs" / "*.jar"
    )
    dirs.foldLeft(PathFinder.empty)(_ +++ _).classpath
  }

  override def run(logger: ProcessLogger) = for {
    _ <- mkdirs(destination).right
    _ <- {
      val extractor = ArchiveExtractor(logger, destination)
      Right(extractor unzip cache.file).right
    }
  } yield {
    logger info s"[done] ${cache.moduleId} expanded to: $destination"
  }

  private def mkdirs(file: File): Either[CacheExpanderError, Unit] =
    try {
      Right(file.mkdirs())
    } catch {
      case e: Exception =>
        Left(new CacheExpanderError(e))
    }
}

class CacheExpanderError(val cause: Exception)
