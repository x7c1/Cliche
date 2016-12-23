import sbt.{File, ProcessLogger}
import sbt.Path.richFile
import sbt.Process.stringToProcess

class RGenerator(
  logger: ProcessLogger,
  sdk: AndroidSdk,
  destination: File,
  sourceDestination: File) {

  def generateFrom(cache: AarCache): Int = {
    val target = destination.getAbsolutePath

    /*
      --auto-add-overlay
      --non-constant-id
      -v
     */

    val command =
      s"""${sdk.buildTools.getAbsolutePath}/aapt package
         | -m
         | -S $target/res
         | -J ${sourceDestination.getAbsolutePath}
         | -M $target/AndroidManifest.xml
         | -I ${sdk.platforms.absolutePath}/android.jar
         | --generate-dependencies
         | """.stripMargin

    println(command)
    command !< logger
  }
}
