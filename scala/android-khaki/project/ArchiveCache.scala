import sbt.{File, ModuleID}


object ArchiveCache {
  def aar(file: File, moduleID: ModuleID): AarCache = {
    new AarCache(file, moduleID)
  }
}

sealed trait ArchiveCache {

  def file: File

  def moduleId: ModuleID
}

class AarCache(
  override val file: File,
  override val moduleId: ModuleID) extends ArchiveCache
