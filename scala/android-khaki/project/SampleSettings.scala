import KhakiKeys.expand
import sbt.Keys.{streams, thisProject, unmanagedJars, unmanagedSourceDirectories}
import sbt._
import sbtassembly.AssemblyKeys.{assembly, assemblyExcludedJars, assemblyMergeStrategy}
import sbtassembly.MergeStrategy

object KhakiKeys {

  val expand = taskKey[Unit]("expand aar")

  val loadPom = taskKey[Unit]("load pom.xml")
}

object SampleSettings {

  val dependencies = Seq(
    "com.android.support:recyclerview-v7:25.0.1",
    "com.android.support:appcompat-v7:25.0.1",
    "com.android.support:design:25.0.1",
    "com.android.support:cardview-v7:25.0.1",
    "com.android.support:support-compat:25.0.1"
  )

  lazy val sdk = AndroidSdk(
    localProperties = file("local.properties"),
    buildToolsVersion = "23.0.3",
    platformsVersion = "android-25"
  )

  lazy val splicers = Def setting {
    val factory = new ArchiveCacheSplicers.Factory(
      cacheDirectory = sdk.extras / "android/m2repository",
      unmanagedDirectory = thisProject.value.base / "libs-generated",
      sdk = sdk
    )
    factory create dependencies
  }

  def excludeFromAssembly(path: String): Boolean = {
    val `R.java` = ".*/R(\\$[^.]+)?.class$"
    path matches `R.java`
  }

  def tasks: Seq[SettingsDefinition] = Seq(
    expand := {
      splicers.value runAll streams.value.log
    }
  )

  def settings: Seq[SettingsDefinition] = Seq(
    (unmanagedSourceDirectories in Compile) ++= {
      splicers.value.sourceDirectories
    },
    (unmanagedJars in Compile) ++= {
      splicers.value.classpath
    },
    assemblyExcludedJars in assembly ++= {
      splicers.value.classpath
    },
    assemblyMergeStrategy in assembly ~= (original => {
      case path if excludeFromAssembly(path) => MergeStrategy.discard
      case path => original(path)
    })
  )

  def all: Seq[SettingsDefinition] = {
    tasks ++ settings
  }

}
