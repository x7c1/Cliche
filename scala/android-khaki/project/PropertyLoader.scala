import Extractor.==>
import sbt.File

object PropertyLoader {

  def dependencies(file: File): Seq[String] = {
    val lines = io.Source.fromFile(file).getLines() collect {
      case toDependency(line) => line
    }
    lines.toSeq
  }

  import sbt.complete.DefaultParsers._

  private val quoted1 = "'" ~> NotSpace <~ "'"

  private val quoted2 = '"' ~> NotSpace <~ '"'

  private val toDependency: String ==> String = Extractor {
    val dependency = Space.+ ~> "compile" ~> Space.+ ~> (quoted1 | quoted2)
    line => parse(line, dependency).right.toOption
  }
}
