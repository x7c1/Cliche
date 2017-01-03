import KhakiKeys.{dependencies, expand, khaki, sdk}
import sbt.Configurations.config
import sbt.Keys.{streams, thisProject, unmanagedJars, unmanagedSourceDirectories}
import sbt._

object KhakiKeys {
  val khaki = config("khaki")

  val dependencies = settingKey[Seq[String]]("dependencies")

  val expand = taskKey[Unit]("expand aar and jars")

  val sdk = settingKey[AndroidSdk]("Android SDK")
}

object SampleSettings {

  lazy val splicers = Def setting {
    val factory = new ArchiveCacheSplicers.Factory(
      cacheDirectory = (sdk in khaki).value.extras / "android/m2repository",
      unmanagedDirectory = thisProject.value.base / "libs-generated",
      sdk = (sdk in khaki).value
    )
    factory create (dependencies in khaki).value
  }

  def tasks: Seq[SettingsDefinition] = Seq(
    expand in khaki := {
      splicers.value runAll streams.value.log
    }
  )

  def settings: Seq[SettingsDefinition] = Seq(
    (unmanagedSourceDirectories in Compile) ++= {
      splicers.value.sourceDirectories
    },
    (unmanagedJars in Compile) ++= {
      splicers.value.classpath
    }
  )

  def all: Seq[SettingsDefinition] = {
    tasks ++ settings
  }

}
