import sbt.Def.Classpath
import sbt.{Attributed, File, ModuleID, ProcessLogger, globFilter, singleFileFinder}


class ArchiveCacheSplicers private(sdk: AndroidSdk, splicers: Seq[ArchiveCacheSplicer]) {

  def runAll(logger: ProcessLogger): Unit = {
    splicers foreach (_ setupJars logger)
    splicers foreach (_ setupSources logger)
  }

  def classpath: Classpath = {
    val empty: Classpath = Attributed blankSeq Seq()
    splicers.foldLeft(empty)(_ ++ _.loadClasspath) ++ (sdk.platforms * "*.jar").classpath
  }

  def sourceDirectories: Seq[File] = {
    val empty: Seq[File] = Seq()
    splicers.foldLeft(empty)(_ ++ _.sourceDirectories)
  }
}

object ArchiveCacheSplicers {

  class Factory(cacheDirectory: File, unmanagedDirectory: File, sdk: AndroidSdk) {

    def create(dependencies: Seq[String]): ArchiveCacheSplicers = {
      val create = new ArchiveCacheSplicer.Factory(cacheDirectory, unmanagedDirectory, sdk)
      val caches = dependencies map ModuleIdFactory.create map toCache
      new ArchiveCacheSplicers(
        sdk = sdk,
        splicers = filter(caches) map create.fromCache
      )
    }

    private val finder = new ArchiveCacheFinder(cacheDirectory)

    private def toCache(moduleID: ModuleID): ArchiveCache = {
      finder fromModule moduleID match {
        case Left(error) => throw new IllegalArgumentException(error.message)
        case Right(cache) => cache
      }
    }

    private def filter(caches: Seq[ArchiveCache]): Seq[ArchiveCache] = {
      val traverser = ArchiveCacheTraverser(cacheDirectory)
      traverser resolveAll caches match {
        case Left(error) => throw new IllegalArgumentException(error.message)
        case Right(xs) => xs
      }
    }

  }

}
