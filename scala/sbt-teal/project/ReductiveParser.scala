import sbt.complete.DefaultParsers.{NotSpace, Space, failure, token}
import sbt.complete.Parser

import scala.util.{Failure, Success, Try}

object ReductiveParser {
  def from(items: Seq[String]): Parser[Seq[String]] = {
    new ReductiveParser(items).parser
  }
}

class ReductiveParser private (items: Seq[String]) {

  private type Filter = String => Boolean

  def parser: Parser[Seq[String]] = {
    val fixed: Parser[Filter] = {
      val base = items map (token(_)) reduceOption (_ | _)
      base getOrElse failure("none") map (item => _ == item)
    }
    val manually: Parser[Filter] = {
      val base = NotSpace
      base map (input => _ matches input)
    }
    (Space ~> (fixed | manually) flatMap next) ?? Nil
  }

  private def next(filter: Filter): Parser[Seq[String]] =
    Try(items partition filter) match {
      case Success((consumed, remains)) if consumed.nonEmpty =>
        ReductiveParser from remains map (consumed ++ _)
      case Success(_) =>
        failure(s"input not matched")
      case Failure(e) =>
        failure(s"invalid input: ${e.getMessage}")
    }
}
