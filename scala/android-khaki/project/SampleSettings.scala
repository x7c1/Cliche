import Extractor.==>
import KhakiKeys.{khaki, splice, splicerDependencies, splicerSdk, unmanagedDirectory}
import sbt.Configurations.config
import sbt.Keys.{streams, unmanagedJars, unmanagedSourceDirectories}
import sbt._

object KhakiKeys {
  val khaki = config("khaki")

  val splicerDependencies = settingKey[Seq[String]]("dependencies")

  val unmanagedDirectory = settingKey[File]("unmanaged jars directory")

  val splicerSdk = settingKey[AndroidSdk]("Android SDK")

  val splice = taskKey[Unit]("expand archives according to dependencies")
}

object SampleSettings {

  lazy val splicers = Def setting {
    val factory = new ArchiveCacheSplicers.Factory(
      cacheDirectory = splicerSdk.value.extras / "android/m2repository",
      unmanagedDirectory = (unmanagedDirectory in khaki).value,
      sdk = splicerSdk.value
    )
    factory create splicerDependencies.value
  }

  def tasks: Seq[SettingsDefinition] = Seq(
    splice := {
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

object DependenciesLoader {

  def loadFrom(file: File): Seq[String] = {
    val lines = io.Source.fromFile(file).getLines() collect {
      case toDependency(line) => line
    }
    lines.toSeq
  }

  private val toDependency: String ==> String = Extractor {
    import sbt.complete.DefaultParsers._
    val dependency = {
      val quoted1 = "'" ~> NotSpace <~ "'"
      val quoted2 = '"' ~> NotSpace <~ '"'
      Space.+ ~> "compile" ~> Space.+ ~> (quoted1 | quoted2)
    }
    line =>
      parse(line, dependency).right.toOption
  }

}
