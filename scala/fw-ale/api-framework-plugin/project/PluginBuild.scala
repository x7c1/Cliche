import sbt._
import sbt.Keys._

object PluginBuild extends Build {

  val plugin = project.in(file(".")).settings(
    organization := "x7c1",
    name := "SampleFrameworkPlugin",
    version := "0.1",
    scalaVersion := "2.10.4",
    sbtPlugin := true
  )

}
