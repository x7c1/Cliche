import sbt._
import Keys._

object SlateBuild extends Build {

  val slateSettings = Seq(
    scalaVersion := "2.11.2",
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature"
    ),
    libraryDependencies ++= Seq(
      "org.specs2" %% "specs2" % "2.3.12" % "test"
    )
  )
  lazy val `slate-app` = project.
    settings(slateSettings:_*).
    dependsOn(`slate-lib`)

  lazy val `slate-lib` = project.
    settings(slateSettings:_*)

  lazy val root = Project("slate", file(".")).
    aggregate(
      `slate-app`,
      `slate-lib`
    )
}

