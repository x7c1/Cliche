import sbt.{File, ModuleID}


object ArchiveCache {
  def aar(file: File, pom: File, moduleID: ModuleID): AarCache = {
    new AarCache(file, pom, moduleID)
  }

  def jar(file: File, pom: File, moduleID: ModuleID): JarCache = {
    new JarCache(file, pom, moduleID)
  }
}

sealed trait ArchiveCache {

  def file: File

  def pom: File

  def moduleId: ModuleID

  def isSameModule(cache: ArchiveCache) = {
    Seq(
      cache.moduleId.organization == moduleId.organization,
      cache.moduleId.name == moduleId.name,
      cache.moduleId.revision == moduleId.revision
    ) forall (_ == true)
  }
}

class AarCache(
  override val file: File,
  override val pom: File,
  override val moduleId: ModuleID) extends ArchiveCache

class JarCache(
  override val file: File,
  override val pom: File,
  override val moduleId: ModuleID) extends ArchiveCache
