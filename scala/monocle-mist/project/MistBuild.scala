import sbt._
import Keys._

object MistBuild extends Build {

  val mistSettings = Seq(
    scalaVersion := "2.11.6",
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature"
    ),
    libraryDependencies ++= Seq(
      "org.scalatest" % "scalatest_2.11" % "2.2.4" % Test
    )
  )

  val monocleDependencies = {
    val version = "1.2.0-M1"
    Seq(
      "com.github.julien-truffaut"  %%  "monocle-core"    % version,
      "com.github.julien-truffaut"  %%  "monocle-generic" % version,
      "com.github.julien-truffaut"  %%  "monocle-macro"   % version,
      "com.github.julien-truffaut"  %%  "monocle-state"   % version,
      "com.github.julien-truffaut"  %%  "monocle-law"     % version % "test"
    )
  }

  lazy val `mist-app` = project.
    settings(mistSettings:_*).
    dependsOn(`mist-lib`)

  lazy val `mist-lib` = project.
    settings(
      libraryDependencies ++= monocleDependencies,
      libraryDependencies += compilerPlugin(
        "org.scalamacros" %% "paradise" % "2.0.1" cross CrossVersion.full
      )
    ).
    settings(mistSettings:_*)

  lazy val root = Project("mist", file(".")).
    aggregate(
      `mist-app`,
      `mist-lib`
    )
}
