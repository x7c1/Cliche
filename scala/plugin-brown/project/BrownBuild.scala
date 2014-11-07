import sbt._
import Keys._

object BrownBuild extends Build {

  val brownSettings = Seq(
    scalaVersion := "2.11.2",
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature"
    ),
    libraryDependencies ++= Seq(
      "org.scalatest" % "scalatest_2.11" % "2.2.1" % Test
    )
  )
  lazy val `brown-app` = project.
    settings(brownSettings:_*).
    settings(taskSettings:_*).
    settings(scalacOptions <++= (packageBin in Compile in `brown-lib`).map{ jar =>
      Seq(
        "-Xplugin:" + jar.absolutePath,
        "-Jdummy=" + jar.lastModified )
    }).
    dependsOn(`brown-lib`)

  lazy val `brown-lib` = project.
    settings(brownSettings:_*).
    settings(libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-compiler" % scalaVersion.value
    ))

  lazy val root = Project("brown", file(".")).
    aggregate(
      `brown-app`,
      `brown-lib`
    )

  def taskSettings = {
    val recompile = TaskKey[Unit]("recompile", "Execute `clean` and `compile` in order")
    val logger = ConsoleLogger()
    Seq(
      recompile := { logger info "Done recompiling." }
      ,recompile <<= recompile dependsOn (compile in Compile)
      ,recompile <<= recompile dependsOn (clean in Compile)
    )
  }
}
