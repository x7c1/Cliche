import sbt._

import scala.io.Source


trait AndroidSdk {
  def root: File

  def buildTools: File

  def platforms: File

  def extras: File
}

object AndroidSdk {

  def apply(
    localProperties: File,
    buildToolsVersion: String,
    compileSdkVersion: Int): AndroidSdk = {

    val root = file(loadPath(localProperties))
    AndroidSdkImpl(
      root = validate(root),
      buildTools = validate(root / "build-tools" / buildToolsVersion),
      platforms = validate(root / "platforms" / s"android-$compileSdkVersion"),
      extras = validate(root / "extras")
    )
  }

  private def validate(file: File): File = {
    if (file.exists()) {
      file
    } else {
      throw new IllegalArgumentException(s"file not found: $file")
    }
  }

  def loadPath(localProperties: File): String = {
    val lines = Source.fromFile(localProperties).getLines()
    val regex = "^sdk.dir=(.*)".r
    lines collectFirst { case regex(path) => path } getOrElse {
      throw new IllegalStateException("sdk.dir not found")
    }
  }

  private case class AndroidSdkImpl(
    root: File,
    buildTools: File,
    platforms: File,
    extras: File) extends AndroidSdk

}
