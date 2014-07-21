import sbt._
import Keys._

object SlateBuild extends Build {

  val slateSettings = Seq(
    libraryDependencies ++= Seq(
      "org.specs2" %% "specs2" % "2.3.10" % "test"
    )
  )
  lazy val slateApplication = Project(
    "slate-application",
    file("slate-app")).dependsOn(slateLibrary)

  lazy val slateLibrary = Project(
    "slate-library",
    file("slate-lib")).settings(slateSettings:_*)

  lazy val root =
    Project("slate", file(".")).aggregate(
      slateApplication,
      slateLibrary )
}

