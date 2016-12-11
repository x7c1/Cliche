import sbt.complete.DefaultParsers._
import sbt.complete.{FixedSetExamples, Parser}

object SampleParser {

  def select1(items: Iterable[String]) =
    token(Space ~> StringBasic.examples(FixedSetExamples(items)))

  def selectSome(items: Seq[String]): Parser[Seq[String]] = {
    select1(items).flatMap { v ⇒
      val remaining = items filter {
        _ != v
      }
      if (remaining.size == 0)
        success(v :: Nil)
      else
        selectSome(remaining).?.map(v +: _.getOrElse(Seq()))
    }
  }
}

