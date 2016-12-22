import sbt.{File, ModuleID}

class CacheSplicersFactory(cacheDirectory: File, unmanagedDirectory: File) {

  def create(dependencies: Seq[String]): Seq[ArchiveCacheSplicer] = {
    val create = new ArchiveCacheSplicer.Factory(cacheDirectory, unmanagedDirectory)
    val caches = dependencies map ModuleIdFactory.create map toCache
    filter(caches) map create.fromCache
  }

  private val finder = new ArchiveCacheFinder(cacheDirectory)

  private def toCache(moduleID: ModuleID): ArchiveCache = {
    finder search moduleID match {
      case Left(error) => throw new IllegalArgumentException(error.message)
      case Right(cache) => cache
    }
  }

  private def filter(caches: Seq[ArchiveCache]): Seq[ArchiveCache] = {
    val traverser = ArchiveCacheTraverser(cacheDirectory)
    traverser traverse caches match {
      case Left(error) => throw new IllegalArgumentException(error.message)
      case Right(xs) => xs
    }
  }

}
