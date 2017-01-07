import KhakiKeys.{splice, splicerDependencies, splicerSdk}
import PropertyLoader.{buildToolsVersion, compileSdkVersion}
import sbtassembly.AssemblyKeys.{assemblyJarName, assemblyOption, assemblyOutputPath}

lazy val sample: Project = project.
  settings(
    unmanagedJars in Compile ++= {
      (assemblyOutputPath in assembly in `android-jars`).value.get.classpath
    },
    assemblyExcludedJars in assembly ++= {
      (assemblyOutputPath in assembly in `android-jars`).value.get.classpath
    },
    assemblyOption in assembly ~= {
      _ copy (includeScala = false)
    }
  )

lazy val `android-jars` = project.
  settings(SampleSettings.all: _*).
  settings(
    splicerSdk := AndroidSdk(
      localProperties = file("local.properties"),
      buildToolsVersion = buildToolsVersion via file("build.gradle"),
      compileSdkVersion = compileSdkVersion via file("build.gradle")
    ),
    unmanagedBase in splice := {
      thisProject.value.base / "libs-expanded"
    },
    splicerDependencies := {
      PropertyLoader dependencies file("targets.gradle")
    },
    clean := {
      FileCleaner remove (assemblyOutputPath in assembly).value
      clean.value
    }
  ).
  settings(
    assemblyOption in assembly ~= {
      _ copy (includeScala = false)
    },
    assemblyOutputPath in assembly := {
      sample.base / "libs-generated" / (assemblyJarName in assembly).value
    }
  )

lazy val root = Project(id = "root", base = file("."))
