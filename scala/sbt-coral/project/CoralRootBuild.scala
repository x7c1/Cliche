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
    settings(CoralTask.settings:_*).
    settings(
      CoralTask.hello ~= { _ => println("(successfully greeted)")}
    )
}
