import sbt._
import Keys._

object NoirBuild extends Build {

  val noirSettings = Seq(
    scalaVersion := "2.11.2",
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature"
    ),
    libraryDependencies ++= Seq(
      "org.scalatest" % "scalatest_2.11" % "2.2.1" % Test
    )
  )
  lazy val `noir-app` = project.
    settings(noirSettings:_*).
    dependsOn(`noir-lib`)

  lazy val `noir-lib` = project.
    settings(noirSettings:_*).
    settings(libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2"
    ))

  lazy val root = Project("noir", file(".")).
    aggregate(
      `noir-app`,
      `noir-lib`
    )
}

