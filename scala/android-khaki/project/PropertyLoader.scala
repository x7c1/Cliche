import Extractor.==>
import sbt.File
import sbt.complete.Parser

object PropertyLoader {

  import sbt.complete.DefaultParsers._

  def dependencies(file: File): Seq[String] = {
    val parser = quoted
    Loader(file, parser) loadMultiple "compile"
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
      sbt.IO.readLines(file) collect {
        case pattern(line) => line
      }
    }

    private def toPattern(property: String): String ==> String = Extractor {
      val parser = Space ~> property ~> Space ~> target
      line => parse(line, parser).right.toOption
    }

  }

}
