import sbt._
import Keys._

object CoralBulid extends Build{

  lazy val root = Project(
    "root",
    file(".")).settings().aggregate(coral, coralLibrary)

  lazy val coral = Project(
    "coral",
    file("coral")).dependsOn(coralLibrary)

  lazy val coralLibrary = Project(
    "coral-library",
    file("coral-lib"))

}
