import sbt._
import Keys._

object ParseBuild extends Build {

  val parseSettings = Seq(
    scalaVersion := "2.11.2",
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature"
    ),
    libraryDependencies ++= Seq(
      "com.typesafe.play" % "play-ws_2.11" % "2.4.0-M1",
      "io.spray" %%  "spray-json" % "1.3.1",
      "org.scalatest" % "scalatest_2.11" % "2.2.1" % Test
    )
  )
  lazy val `parse-app` = project.
    settings(parseSettings:_*).
    dependsOn(`parse-lib`)

  lazy val `parse-lib` = project.
    settings(parseSettings:_*)

  lazy val root = Project("parse", file(".")).
    aggregate(
      `parse-app`,
      `parse-lib`
    )
}

