import sbt.Keys.{streams, unmanagedJars, unmanagedSourceDirectories}
import sbt._

object Khaki {

  lazy val expand = taskKey[Unit]("expand aar")

  lazy val loadPom = taskKey[Unit]("load pom.xml")

  def all(project: File): Seq[Def.SettingsDefinition] = SampleSettings(project).all
}

case class SampleSettings(project: File) {

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

  lazy val splicers = {
    val factory = new CacheSplicersFactory(
      cacheDirectory = sdk.extras / "android/m2repository",
      unmanagedDirectory = project / "libs-generated",
      sdk = sdk
    )
    factory create dependencies
  }

  def all: Seq[SettingsDefinition] = Seq(
    Khaki.expand := {
      splicers runAll streams.value.log
    },
    (unmanagedSourceDirectories in Compile) ++= {
      splicers.loadAllSourceDirectories
    },
    (unmanagedJars in Compile) ++= {
      splicers.loadAllClasspath ++ (sdk.platforms * "*.jar").classpath
    }
  )

}
