import sbt._
import Keys._

object LilacBuild extends Build {

  val lilacSettings = Seq(
    scalaVersion := "2.11.2",
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature"
    ),
    libraryDependencies ++= Seq(
      "org.specs2" %% "specs2" % "2.3.12" % "test"
    )
  )
  lazy val `lilac-app` = project.
    settings(lilacSettings:_*).
    dependsOn(`lilac-lib`)

  lazy val `lilac-lib` = project.
    settings(lilacSettings:_*).
    settings(libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2"
    ))

  lazy val root = Project("lilac", file(".")).
    aggregate(
      `lilac-app`,
      `lilac-lib`
    )
}
