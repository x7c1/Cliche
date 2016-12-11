import sbt.complete.DefaultParsers.{Space, failure, token}
import sbt.complete.Parser

object StrictParser {
  def from(items: Seq[String]): Parser[Seq[String]] = {
    new StrictParser(items).parser
  }
}

class StrictParser private (items: Seq[String]) {

  private type Filter = String => Boolean

  def parser: Parser[Seq[String]] = {
    val fixed: Parser[Filter] = {
      val base = items map (token(_)) reduceOption (_ | _)
      base getOrElse failure("none") map (item => _ == item)
    }
    (Space ~> fixed flatMap next) ?? Nil
  }

  private def next(filter: Filter): Parser[Seq[String]] = {
    val (consumed, remains) = items partition filter
    if (consumed.nonEmpty){
      ReductiveParser from remains map (consumed ++ _)
    } else {
      failure("input not matched")
    }
  }
}
