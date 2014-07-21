import coral.plugin.CoralBuildPlugin.CoralTask
import sbt._
import Keys.libraryDependencies

object CoralRootBuild extends Build{

  def coralSettings = Seq(
    libraryDependencies ++= Seq(
      "org.specs2" %% "specs2" % "2.3.10" % "test"
    )
  )

  lazy val coral = Project(
    "coral",
    file("coral")).dependsOn(coralLibrary)

  lazy val coralLibrary = Project(
    "coral-library",
    file("coral-lib")).settings(coralSettings:_*)

  lazy val root = Project("root", file(".")).
    aggregate(coral, coralLibrary).
    settings(CoralSampleTasks.toSeq:_*)

    /*
    intellij cannot resolve these dependencies
    .settings(CoralTask.settings:_*)
    .settings(
      CoralTask.hello ~= { _ => println("(successfully greeted)")})
    */

  object CoralSampleTasks {
    val dev = config("dev") describedAs "development config"

    val prod = config("prod") describedAs "production config"

    val endpoint = SettingKey[String]("endpoint", "api endpoint")

    val say = TaskKey[Unit]("say", "a simple task experiment")

    val sayTask = say <<= endpoint map { endpoint =>
      println("endpoint : " + endpoint)
    }
    def commonTasks = Seq(sayTask)

    def configValues = Seq(
      endpoint in dev := "dev.example.com",
      endpoint in prod := "prod.example.com" )

    def toSeq = configValues ++
      inConfig(dev)(commonTasks) ++
      inConfig(prod)(commonTasks)

  }

}
