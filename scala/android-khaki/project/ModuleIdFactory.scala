import sbt.ModuleID

object ModuleIdFactory {

  /*
    e.g.
    target = "com.android.support:recyclerview-v7:25.0.1"
   */
  def create(target: String): ModuleID = {
    val Array(groupId, artifactId, version) = target.split(':')
    ModuleID(
      organization = groupId,
      name = artifactId,
      revision = version
    )
  }
}
