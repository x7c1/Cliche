import sbt._
import Keys._

object ColorfulBuild extends Build {

  val colorfulSettings = Seq(
    scalaVersion := "2.11.6",
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature"
    ),
    libraryDependencies ++= Seq(
      "org.scalatest" % "scalatest_2.11" % "2.2.4" % Test
    )
  )
  lazy val `colorful-app` = project.
    settings(colorfulSettings:_*).
    dependsOn(`colorful-lib`)

  lazy val `colorful-lib` = project.
    settings(colorfulSettings:_*).
    dependsOn(ProjectRef(
      uri("git://github.com/fpinscala/fpinscala.git#fe5bca4339"),
      "answers"
    ))

  lazy val root = Project("colorful", file(".")).
    aggregate(
      `colorful-app`,
      `colorful-lib`
    )
}
