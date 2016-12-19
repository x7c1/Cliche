import sbt.{File, ModuleID}
import sbt._

class ArchiveCacheFinder(cacheDirectory: File) {

  def find(moduleId: ModuleID): Option[ArchiveCache] = {
    val directory = cacheDirectory /
      moduleId.organization.replace(".", "/") /
      moduleId.name /
      moduleId.revision

    directory / s"${moduleId.name}-${moduleId.revision}.aar" match {
      case archive if archive.exists() =>
        Some apply ArchiveCache.aar(archive, moduleId)
      case _ =>
        None
    }
  }
}
