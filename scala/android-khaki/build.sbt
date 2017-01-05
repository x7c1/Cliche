import KhakiKeys.{dependencies, khaki, sdk, unmanagedDirectory}
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
    sdk in khaki := AndroidSdk(
      localProperties = file("local.properties"),
      buildToolsVersion = "23.0.3",
      compileSdkVersion = 25
    ),
    unmanagedDirectory in khaki := {
      thisProject.value.base / "libs-expanded"
    },
    dependencies in khaki := {
      DependenciesLoader loadFrom file("targets.gradle")
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
