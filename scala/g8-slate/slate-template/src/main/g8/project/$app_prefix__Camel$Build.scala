import sbt._
import Keys._

object $app_prefix;format="Camel"$Build extends Build {

  val $app_prefix$Settings = Seq(
    scalaVersion := "2.11.2",
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature"
    ),
    libraryDependencies ++= Seq(
      "org.specs2" %% "specs2" % "2.3.12" % "test"
    )
  )
  lazy val `$app_prefix$-app` = project.
    settings($app_prefix$Settings:_*).
    dependsOn(`$app_prefix$-lib`)

  lazy val `$app_prefix$-lib` = project.
    settings($app_prefix$Settings:_*)

  lazy val root = Project("$app_prefix$", file(".")).
    aggregate(
      `$app_prefix$-app`,
      `$app_prefix$-lib`
    )
}

