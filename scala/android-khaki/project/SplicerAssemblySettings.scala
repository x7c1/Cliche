import SplicerKeys.{splice, splicerDependencies, splicerSdk}
import PropertyLoader.{buildToolsVersion, compileSdkVersion, dependencies, sdkRoot}
import sbt.Def.{SettingList, SettingsDefinition}
import sbt.Keys.{clean, thisProject, unmanagedBase, unmanagedJars}
import sbt.{File, Project, ResolvedProject, richFile, richFiles, singleFileFinder}
import sbtassembly.AssemblyKeys.{assembly, assemblyJarName, assemblyOption, assemblyOutputPath}
import sbtassembly.AssemblyPlugin.autoImport.assemblyExcludedJars


object SplicerAssemblySettings {

  def forProvider(
    assemblyDirectory: => File,
    splicerDirectory: ResolvedProject => File,
    localProperties: File,
    buildGradle: File,
    dependenciesGradle: File): SettingsDefinition = {

    val settings = Seq(
      splicerSdk := AndroidSdk(
        sdkRoot = sdkRoot via localProperties,
        buildToolsVersion = buildToolsVersion via buildGradle,
        compileSdkVersion = compileSdkVersion via buildGradle
      ),
      unmanagedBase in splice := {
        splicerDirectory(thisProject.value)
      },
      splicerDependencies := {
        dependencies via dependenciesGradle
      },
      clean := {
        FileCleaner remove (assemblyOutputPath in assembly).value
        clean.value
      },
      assemblyOption in assembly ~= {
        _ copy (includeScala = false)
      },
      assemblyOutputPath in assembly := {
        assemblyDirectory / (assemblyJarName in assembly).value
      }
    )
    new SettingList(SplicerSettings.all ++ settings)
  }

  def forClient(providerProject: => Project): SettingsDefinition = {
    val settings = Seq(
      unmanagedJars in sbt.Compile ++= {
        (assemblyOutputPath in assembly in providerProject).value.get.classpath
      },
      assemblyExcludedJars in assembly ++= {
        (assemblyOutputPath in assembly in providerProject).value.get.classpath
      },
      assemblyOption in assembly ~= {
        _ copy (includeScala = false)
      }
    )
    new SettingList(settings)
  }
}
