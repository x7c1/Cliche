import Extractor.==>
import sbt.Def.Classpath
import sbt.Path.richFile
import sbt.{File, Logger, PathFinder, globFilter, singleFileFinder}


sealed trait ArchiveCacheSplicer {

  def setupJars: Reader[Logger, Unit]

  def setupSources: Reader[Logger, Unit]

  def clean: Reader[Logger, Unit]

  def loadClasspath: Classpath

  def sourceDirectories: Seq[File]
}

object ArchiveCacheSplicer {

  class Factory(cacheDirectory: File, unmanagedDirectory: File, sdk: AndroidSdk) {
    def fromCache(cache: ArchiveCache): ArchiveCacheSplicer = {
      cache match {
        case aar: AarCache =>
          new AarCacheExpander(cacheDirectory, unmanagedDirectory, sdk, aar)
        case jar: JarCache =>
          new JarCacheLoader(cacheDirectory, jar)
        case unknown =>
          val name = unknown.getClass.getName
          throw new IllegalArgumentException(
            s"unknown archive type [$name] : ${cache.moduleId}")
      }
    }
  }

}

class AarCacheExpander(
  cacheDirectory: File,
  unmanagedDirectory: File,
  sdk: AndroidSdk,
  cache: AarCache) extends ArchiveCacheSplicer {

  private val destination = {
    unmanagedDirectory / cache.moduleId.name
  }
  private val sourceDestination = {
    destination / "src-generated"
  }
  private val manifest = {
    destination / "AndroidManifest.xml"
  }

  override def loadClasspath = {
    val finders = Seq(
      PathFinder(destination / "classes.jar"),
      destination / "libs" * "*.jar"
    )
    finders.foldLeft(PathFinder.empty)(_ +++ _).classpath
  }

  override def sourceDirectories = {
    Seq(sourceDestination.getAbsoluteFile)
  }

  override def setupJars = Reader { logger =>
    val either = for {
      _ <- mkdirs(destination).right
      code <- {
        val extractor = ArchiveExtractor(logger, destination)
        Right(extractor unzip cache.file).right
      }
    } yield code match {
      case 0 =>
        logger info s"[done] ${cache.moduleId} expanded -> $destination"
      case n =>
        logger error s"(code:$n) failed to extract ${cache.file}"
    }
    either.left foreach (logger error _.message)
  }

  override def setupSources = Reader { logger =>
    val either = for {
      _ <- mkdirs(sourceDestination).right
      dirs <- resourceDirectories.right
      code <- {
        val generator = new RGenerator(logger, sdk, manifest, sourceDestination)
        Right(generator generateFrom dirs).right
      }
    } yield code match {
      case 0 => (sourceDestination ** "*.java").get match {
        case files if files.nonEmpty =>
          files foreach (logger info s"[done] generated -> " + _)
        case none =>
          logger info s"[done] no output: ${cache.moduleId}"
      }
      case n =>
        logger error s"(code:$n) failed to generate R.java: ${cache.moduleId}"
    }
    either.left foreach (logger error _.message)
  }

  private val traverser = ArchiveCacheTraverser(cacheDirectory)

  private val notFound: Seq[File] ==> File = Extractor {
    _ find (!_.exists())
  }

  private def resourceDirectories: Either[CacheSplicerError, Seq[File]] = {
    val caches = traverser.resolve(cache).left map convertError
    caches.right map toResDirectories match {
      case Right(notFound(res)) => Left(CacheSplicerError.NotFound(res))
      case x => x
    }
  }

  private def convertError(e: FinderError) = {
    CacheSplicerError.Messaged(s"${e.getClass.getSimpleName}: ${e.message}")
  }

  private def toResDirectories(caches: Seq[ArchiveCache]) = {
    caches collect { case aar: AarCache => aar } map {
      unmanagedDirectory / _.moduleId.name / "res"
    }
  }

  private def mkdirs(file: File): Either[CacheSplicerError, Unit] =
    try {
      Right(file.mkdirs())
    } catch {
      case e: Exception =>
        Left(CacheSplicerError.Unexpected(e))
    }

  override def clean = {
    FileCleaner.withLogging remove destination
  }
}

class JarCacheLoader(
  cacheDirectory: File,
  cache: JarCache) extends ArchiveCacheSplicer {

  override def setupJars = Reader { logger =>
    logger info s"[done] skipped: ${cache.moduleId} jar found: ${cache.file}"
  }

  override def setupSources = Reader { logger =>
    logger info s"[done] skipped: no source to generate: ${cache.file.name}"
  }

  override def loadClasspath: Classpath = {
    PathFinder(cache.file).classpath
  }

  override def sourceDirectories = Seq()

  override def clean = Reader { logger =>
    logger info s"[done] skipped: no files to clean: ${cache.moduleId}"
  }
}
