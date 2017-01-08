import SplicerKeys.{splicerExpand, splicerClean, splicerDependencies, splicerSdk}
import sbt.Keys.{clean, streams, unmanagedBase, unmanagedJars, unmanagedSourceDirectories}
import sbt._

object SplicerKeys {
  val splicerDependencies = settingKey[Seq[String]]("dependencies")

  val splicerSdk = settingKey[AndroidSdk]("Android SDK")

  val splicerExpand = taskKey[Unit]("expand archives according to dependencies")

  val splicerClean = taskKey[Unit]("delete expanded files")
}

object SplicerSettings {

  private lazy val splicers = Def setting {
    val factory = new ArchiveCacheSplicers.Factory(
      cacheDirectory = splicerSdk.value.extras / "android/m2repository",
      unmanagedDirectory = (unmanagedBase in splicerExpand).value,
      sdk = splicerSdk.value
    )
    factory create splicerDependencies.value
  }

  private def tasks = Seq(
    splicerClean := {
      splicers.value cleanAll streams.value.log
    },
    splicerExpand := {
      splicers.value runAll streams.value.log
    },
    clean := {
      clean.value
      splicerClean.value
    }
  )

  private def settings = Seq(
    (unmanagedSourceDirectories in Compile) ++= {
      splicers.value.sourceDirectories
    },
    (unmanagedJars in Compile) ++= {
      splicers.value.classpath
    }
  )

  def all: SettingsDefinition = {
    new sbt.Def.SettingList(tasks ++ settings)
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
