import sbt._
import Keys._

object CyanBuild extends Build {

  val cyanSettings = Seq(
    scalaVersion := "2.11.2",
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature"
    )
  )
  lazy val `cyan-app` = project.
    settings(cyanSettings:_*).
    dependsOn(`cyan-lib`)

  lazy val `cyan-lib` = project.
    settings(cyanSettings:_*).
    settings(libraryDependencies ++= Seq(
      "log4j" % "log4j" % "1.2.14",
      "com.typesafe.play" % "play-ws_2.11" % "2.4.0-M1",
      "org.scala-lang.modules" %% "scala-xml" %  "1.0.2",
      "org.specs2" %% "specs2" % "2.3.12" % "test"
    ))

  lazy val root = Project("cyan", file(".")).
    aggregate(`cyan-app`, `cyan-lib`)
}
