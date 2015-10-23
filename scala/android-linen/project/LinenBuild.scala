import sbt._
import Keys._

import sbtassembly.AssemblyKeys.{assemblyOutputPath, assembly, assemblyJarName}

import scala.io.Source

object LinenBuild extends Build {

  val linenSettings = Seq(
    scalaVersion := "2.11.6",
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature"
    ),
    libraryDependencies ++= Seq(
      "org.scalatest" % "scalatest_2.11" % "2.2.4" % Test
    )
  )
  lazy val `linen-app` = project.
    settings(unmanagedBase := baseDirectory.value / "custom-lib").
    settings(linenSettings:_*)

  lazy val `linen-struct` = project.
    settings(linenSettings:_*)

  lazy val `linen-lib` = project.
    settings(sdkClasspath).
    settings(linenSettings:_*).
    settings(linenJarPath).
    dependsOn(`linen-struct`)

  lazy val root = Project("linen", file(".")).
    aggregate(`linen-app`, `linen-lib`)

  lazy val linenJarPath = assemblyOutputPath in assembly := {
    val jar = (assemblyJarName in assembly).value
    `linen-app`.base / "custom-lib" / jar
  }

  lazy val sdkClasspath = unmanagedJars in Compile := {
    val sdk = {
      val lines = Source.fromFile(file("local.properties")).getLines()
      val regex = "^sdk.dir=(.*)".r
      lines collectFirst { case regex(path) => file(path) } getOrElse {
        throw new IllegalStateException("sdk.dir not found")
      }
    }
    val dirs = {
      val support = "extras/android/support/v7/appcompat/libs"
      val platform = "platforms/android-23"
      (sdk / platform) +++ (sdk / support)
    }
    (dirs ** "*.jar").classpath
  }

}
