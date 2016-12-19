import sbt.{File, ModuleID}


object ArchiveCache {
  def aar(file: File, pom: File, moduleID: ModuleID): AarCache = {
    new AarCache(file, pom, moduleID)
  }
}

sealed trait ArchiveCache {

  def file: File

  def pom: File

  def moduleId: ModuleID
}

class AarCache(
  override val file: File,
  override val pom: File,
  override val moduleId: ModuleID) extends ArchiveCache
