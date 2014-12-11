import sbt._
import Keys._

object TaupeBuild extends Build {

  val taupeSettings = Seq(
    scalaVersion := "2.11.2",
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature"
    ),
    libraryDependencies ++= Seq(
      "org.scalatest" % "scalatest_2.11" % "2.2.1" % Test
    ),
    organization := "x7c1"
  )
  lazy val `taupe-app` = project.
    settings(taupeSettings:_*).
    dependsOn(`taupe-lib`).
    dependsOn(ProjectRef(uri("git://github.com/x7c1/Salad.git#0.1"), "salad-lib"))

  lazy val `taupe-lib` = project.
    settings(taupeSettings:_*)

  lazy val root = Project("taupe", file(".")).
    aggregate(
      `taupe-app`,
      `taupe-lib`
    )
}
