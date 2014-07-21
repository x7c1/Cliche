import sbt._
import Keys._

object PlumBuild extends Build {

  val plumSettings = Seq(
    libraryDependencies ++= Seq(
      "org.specs2" %% "specs2" % "2.3.10" % "test"
    )
  )
  val plumLibrary = Project(
    id = "plum-lib",
    base = file("plum-lib")).settings(plumSettings:_*)

  val plumApplication = {
    val application = Project(
      id = "plum-app",
      base = file("plum-app")
    )
    application
      .dependsOn(plumLibrary)
      .settings(
        libraryDependencies ++= Seq(
          "org.mortbay.jetty" % "jetty" % "6.1.22" % "container",
          "javax.servlet" % "javax.servlet-api" % "3.0.1" % "provided"
        )
      )
      // this line makes intellij's import failed, although compiling by sbt succeeds.
      //.settings(sbtappengine.Plugin.appengineSettings:_*)
  }
  val root = Project(
    id = "plum",
    base = file(".")).aggregate(plumApplication, plumLibrary)
}
