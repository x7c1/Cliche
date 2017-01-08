
import SplicerAssemblySettings.{forClient, forProvider}

lazy val sample: Project = project.
  settings(forClient(
    providerProject = `android-jars`
  ))

lazy val `android-jars` = project.
  settings(forProvider(
    assemblyDirectory = sample.base / "libs-generated",
    splicerDirectory = _.base / "libs-expanded",
    localProperties = file("local.properties"),
    buildGradle = file("build.gradle"),
    dependenciesGradle = file("targets.gradle")
  ))

lazy val root = Project(id = "root", base = file("."))
