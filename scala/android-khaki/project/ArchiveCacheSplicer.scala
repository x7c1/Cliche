import sbt.Def.Classpath
import sbt.Path.richFile
import sbt.{Attributed, File, PathFinder, ProcessLogger}


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

  class Factory(unmanagedDirectory: File) {
    def createFrom(cache: ArchiveCache): ArchiveCacheSplicer = {
      cache match {
        case aar: AarCache =>
          new AarCacheExpander(unmanagedDirectory, aar)
        case unknown =>
          val name = unknown.getClass.getName
          throw new IllegalArgumentException(s"unknown archive type: $name")
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
