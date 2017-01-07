import KhakiKeys.{splice, splicerClean, splicerDependencies, splicerSdk}
import sbt.Keys.{clean, streams, unmanagedBase, unmanagedJars, unmanagedSourceDirectories}
import sbt._

object KhakiKeys {
  val splicerDependencies = settingKey[Seq[String]]("dependencies")

  val splicerSdk = settingKey[AndroidSdk]("Android SDK")

  val splice = taskKey[Unit]("expand archives according to dependencies")

  val splicerClean = taskKey[Unit]("delete expanded files")
}

object SampleSettings {

  lazy val splicers = Def setting {
    val factory = new ArchiveCacheSplicers.Factory(
      cacheDirectory = splicerSdk.value.extras / "android/m2repository",
      unmanagedDirectory = (unmanagedBase in splice).value,
      sdk = splicerSdk.value
    )
    factory create splicerDependencies.value
  }

  def tasks: Seq[SettingsDefinition] = Seq(
    splicerClean := {
      splicers.value cleanAll streams.value.log
    },
    splice := {
      splicers.value runAll streams.value.log
    },
    clean := {
      clean.value
      splicerClean.value
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

object FileCleaner {
  def remove(file: File): Unit = {
    sbt.Defaults.doClean(
      clean = Seq(file),
      preserve = Seq()
    )
  }
}
