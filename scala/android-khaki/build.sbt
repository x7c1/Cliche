import KhakiKeys.{dependencies, khaki}
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
    dependencies in khaki := Seq(
      "com.android.support:recyclerview-v7:25.0.1",
      "com.android.support:appcompat-v7:25.0.1",
      "com.android.support:design:25.0.1",
      "com.android.support:cardview-v7:25.0.1",
      "com.android.support:support-compat:25.0.1"
    )
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
