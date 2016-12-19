import sbt.{File, ModuleID}

class CacheSplicersFactory(cacheDirectory: File, unmanagedDirectory: File) {

  private val finder = new ArchiveCacheFinder(cacheDirectory)

  private val factory = new ArchiveCacheSplicer.Factory(unmanagedDirectory)

  def toCache(moduleID: ModuleID): ArchiveCache = {
    finder search moduleID match {
      case Left(error) =>
        throw new IllegalArgumentException(error.message)
      case Right(cache) =>
        cache
    }
  }

  def toSplicer(cache: ArchiveCache): ArchiveCacheSplicer = {
    factory createFrom cache
  }
}
