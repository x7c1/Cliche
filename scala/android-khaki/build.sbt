
import SplicerAssemblySettings.{forClient, forProvider}

lazy val sample = project.
  settings(forClient(
    providerProject = `android-jars`
  ))

lazy val `android-jars` = project.
  settings(forProvider(
    assemblyDirectory = _.base / "libs-assembled",
    splicerDirectory = _.base / "libs-expanded",
    localProperties = file("local.properties"),
    buildGradle = file("build.gradle"),
    dependenciesGradle = file("targets.gradle")
  ))

lazy val root = Project(id = "root", base = file("."))
