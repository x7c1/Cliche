import sbt.Def.{SettingList, SettingsDefinition}
import sbt.Keys.clean
import sbt.{Def, taskKey}
import sbtassembly.AssemblyKeys.{assembly, assemblyOutputPath}


object SplicerAssemblyClean {
  val splicerAssemblyClean = taskKey[Unit]("remove assembled jar")

  def settings: SettingsDefinition = new SettingList(Seq(
    splicerAssemblyClean := assemblyClean.value,
    clean := clean.dependsOn(splicerAssemblyClean).value
  ))

  private val assemblyClean = Def taskDyn {
    FileCleaner.Readers remove (assemblyOutputPath in assembly).value
  }
}
