import coral.plugin.CoralBuildPlugin.CoralTask
import sbt._

object CoralRootBuild extends Build{

  lazy val coral = Project(
    "coral",
    file("coral")).dependsOn(coralLibrary)

  lazy val coralLibrary = Project(
    "coral-library",
    file("coral-lib"))

  lazy val root = Project("root", file(".")).
    settings(CoralTask.settings:_*).
    settings(
      CoralTask.hello ~= { _ => println("(successfully greeted)")}
    )
}
