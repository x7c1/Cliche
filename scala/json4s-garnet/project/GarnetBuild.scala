import sbt._
import Keys._

object GarnetBuild extends Build {

  val garnetSettings = Seq(
    scalaVersion := "2.11.2",
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature"
    ),
    libraryDependencies ++= Seq(
      "org.scalatest" % "scalatest_2.11" % "2.2.1" % Test
    )
  )
  lazy val `garnet-app` = project.
    settings(garnetSettings:_*).
    dependsOn(`garnet-lib`)

  lazy val `garnet-lib` = project.
    settings(garnetSettings:_*).
    settings(libraryDependencies ++= Seq(
      "org.json4s" %% "json4s-native" % "3.2.11"
    ))

  lazy val root = Project("garnet", file(".")).
    aggregate(
      `garnet-app`,
      `garnet-lib`
    )
}
