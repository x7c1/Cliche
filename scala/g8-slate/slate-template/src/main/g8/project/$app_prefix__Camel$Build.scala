import sbt._
import Keys._

object $app_prefix;format="Camel"$Build extends Build {

  val $app_prefix$Settings = Seq(
    libraryDependencies ++= Seq(
      "org.specs2" %% "specs2" % "2.3.10" % "test"
    )
  )
  lazy val $app_prefix$Application = Project(
    "$app_prefix$-application",
    file("$app_prefix$-app")).dependsOn($app_prefix$Library)

  lazy val $app_prefix$Library = Project(
    "$app_prefix$-library",
    file("$app_prefix$-lib")).settings($app_prefix$Settings:_*)

  lazy val root =
    Project("$app_prefix$", file(".")).aggregate(
      $app_prefix$Application,
      $app_prefix$Library )
}

