import Extractor.==>
import sbt.{File, IO}
import sbt.complete.Parser

import scala.io.Source

object PropertyLoader {

  import sbt.complete.DefaultParsers._

  object sdkRoot {
    def via(localProperties: File): File = {
      new File(loadPath(localProperties))
    }

    private def loadPath(localProperties: File): String = {
      val lines = Source.fromFile(localProperties).getLines()
      val regex = "^sdk.dir=(.*)".r
      lines collectFirst { case regex(path) => path } getOrElse {
        throw new IllegalStateException("sdk.dir not found")
      }
    }

  }

  object buildToolsVersion {
    def via(file: File): String = {
      val parser = quoted
      Loader(file, parser).requireSingle("buildToolsVersion")
    }
  }

  object compileSdkVersion {
    def via(file: File): Int = {
      val parser = Digit.+.string
      Loader(file, parser).requireSingle("compileSdkVersion").toInt
    }
  }

  object dependencies {
    def via(file: File): Seq[String] = {
      val parser = quoted
      Loader(file, parser) loadMultiple "compile"
    }
  }

  private val quoted = {
    val quoted1 = "'" ~> NotSpace <~ "'"
    val quoted2 = '"' ~> NotSpace <~ '"'
    quoted1 | quoted2
  }

  private case class Loader(file: File, target: Parser[String]) {

    def requireSingle(property: String): String = {
      loadMultiple(property) match {
        case x +: Seq() => x
        case x +: xs =>
          val targets = xs mkString ", "
          throw new IllegalArgumentException(s"multiple $property found: $targets")
        case Seq() =>
          throw new IllegalArgumentException(s"$property not found: $file")
      }
    }

    def loadMultiple(property: String): Seq[String] = {
      val pattern = toPattern(property)
      IO.readLines(file) collect {
        case pattern(line) => line
      }
    }

    private def toPattern(property: String): String ==> String = Extractor {
      val parser = Space ~> property ~> Space ~> target
      line => parse(line, parser).right.toOption
    }

  }

}
