
lazy val sample: Project = project.
  settings(SampleSettings.all: _*)

lazy val root = Project(id = "root", base = file("."))
