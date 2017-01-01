
import sbtassembly.AssemblyKeys.{assemblyJarName, assemblyOption, assemblyOutputPath}

lazy val sample: Project = project.
  settings(
    unmanagedJars in Compile ++= {
      val output = (assemblyOutputPath in assembly in `android-jars`).value
      output.get.classpath
    }
  )

lazy val `android-jars` = project.
  settings(SampleSettings.all: _*).
  settings(
    assemblyOption in assembly ~= {
      _ copy (includeScala = false)
    },
    assemblyOutputPath in assembly := {
      sample.base / "libs-generated" / (assemblyJarName in assembly).value
    }
  )

lazy val root = Project(id = "root", base = file("."))
