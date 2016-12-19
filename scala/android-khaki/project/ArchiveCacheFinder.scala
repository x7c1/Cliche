import sbt.{File, ModuleID}
import sbt._

class ArchiveCacheFinder(cacheDirectory: File) {

  def search(moduleId: ModuleID): Either[FinderError, ArchiveCache] = {
    val directory = cacheDirectory /
      moduleId.organization.replace(".", "/") /
      moduleId.name /
      moduleId.revision

    val prefix = s"${moduleId.name}-${moduleId.revision}"

    def loadArchive = directory / s"$prefix.aar" match {
      case x if x.exists() => Right(x)
      case x => Left(FinderError(s"archive not found: $x"))
    }
    def loadPom = directory / s"$prefix.pom" match {
      case x if x.exists() => Right(x)
      case x => Left(FinderError(s"pom not found: $x"))
    }
    for {
      archive <- loadArchive.right
      pom <- loadPom.right
    } yield ArchiveCache.aar(archive, pom, moduleId)
  }
}

case class FinderError(message: String)
