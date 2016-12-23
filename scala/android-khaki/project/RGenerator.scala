import sbt.Process.stringToProcess
import sbt.{File, ProcessLogger}

class RGenerator(
  logger: ProcessLogger,
  sdk: AndroidSdk,
  manifest: File,
  sourceDestination: File) {

  def generateFrom(resourceDirectories: Seq[File]): Int = {

    val dirs = resourceDirectories map
      (_.getAbsolutePath) map
      ("-S " + _) mkString " "

    val command =
      s"""${sdk.buildTools.getAbsolutePath}/aapt package
         | --auto-add-overlay
         | $dirs
         | -m -J ${sourceDestination.getAbsolutePath}
         | -M ${manifest.getAbsolutePath}
         | -I ${sdk.platforms.getAbsolutePath}/android.jar
         | """.stripMargin

//    logger info command
    command !< logger
  }
}
