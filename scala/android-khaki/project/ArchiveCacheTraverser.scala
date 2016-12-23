import sbt.File


object ArchiveCacheTraverser {
  def apply(cacheDirectory: File): ArchiveCacheTraverser = {
    new ArchiveCacheTraverser(cacheDirectory)
  }
}

class ArchiveCacheTraverser private(cacheDirectory: File) {

  def resolve(cache: ArchiveCache): Either[FinderError, Seq[ArchiveCache]] = {
    traverse(Seq(cache))
  }

  def traverse(caches: Seq[ArchiveCache]): Either[FinderError, Seq[ArchiveCache]] = {
    loop(caches, ResolvedCaches.empty)
  }

  private type Caches = Either[FinderError, Seq[ArchiveCache]]

  private def loop(caches: Seq[ArchiveCache], resolved: ResolvedCaches): Caches = {
    caches match {
      case x +: xs if resolved has x => loop(xs, resolved)
      case x +: xs => cachesOf(x) match {
        case Right(ys) => loop(xs ++ ys, resolved add x)
        case Left(error) => Left(error)
      }
      case Nil => Right(resolved.all)
    }
  }

  private val finder = new ArchiveCacheFinder(cacheDirectory)

  private def cachesOf(cache: ArchiveCache): Caches = {
    val project = PomProjectLoader load cache.pom
    project.dependencies.foldLeft(Right(Seq()): Caches) {
      (either, dependency) => for {
        xs <- either.right
        x <- finder.fromDependency(dependency).right
      } yield xs :+ x
    }
  }

}

object ResolvedCaches {
  def empty: ResolvedCaches = new ResolvedCaches()
}

class ResolvedCaches private(
  val all: Seq[ArchiveCache] = Seq()) {

  def has(cache: ArchiveCache): Boolean = {
    all exists cache.isSameModule
  }

  def add(cache: ArchiveCache): ResolvedCaches = {
    if (has(cache)) {
      this
    } else {
      new ResolvedCaches(all :+ cache)
    }
  }

}
