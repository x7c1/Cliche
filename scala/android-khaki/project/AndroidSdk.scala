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
    platformsVersion: String): AndroidSdk = {

    val root = file(loadPath(localProperties))
    AndroidSdkImpl(
      root = root,
      buildTools = root / "build-tools" / buildToolsVersion,
      platforms = root / "platforms" / platformsVersion,
      extras = root / "extras"
    )
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
