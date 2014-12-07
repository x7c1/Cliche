import sbt._
import Keys._

object AleBuild extends Build {

  val aleSettings = Seq(
    scalaVersion := "2.11.2",
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature"
    ),
    libraryDependencies ++= Seq(
      "org.scalatest" % "scalatest_2.11" % "2.2.1" % Test
    )
  )

  lazy val `ale-api` = Project("ale-api", file("./ale-apps/app1-api")).
    settings(aleSettings:_*).
    settings(SampleFrameworkPlugin.sampleSettings:_*).
    dependsOn(`api-framework`).
    dependsOn(`ale-domain`)

  lazy val `api-framework` = Project("api-fw", file("./api-framework")).
    settings(libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "salad-lib" % "salad-lib_2.11" % "0.1-SNAPSHOT"
    )).
    settings(aleSettings:_*)

  lazy val `ale-domain` = Project("ale-domain", file("./ale-domain")).
    settings(aleSettings:_*)

  lazy val `ale-root` = Project("ale-root", file(".")).
    aggregate(
      `api-framework`,
      `ale-api`,
      `ale-domain`
    )
}
