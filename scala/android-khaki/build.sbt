
lazy val sample: Project = project.
  settings(Khaki.all(file("sample")):_*)

lazy val root = Project(id = "root", base = file(".")).
  settings(Khaki.all(sample.base):_*)
