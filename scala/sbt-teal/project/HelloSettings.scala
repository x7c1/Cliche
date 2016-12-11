import sbt.Def
import sbt.Def.inputKey
import sbt.complete.Parser

object HelloSettings {
  lazy val hello = inputKey[Unit]("hello, parser")

  def task = hello := {
    println("selected items..")

//    val items = Def.setting(sampleParser).parsed
//    val items = Def.setting(strictParser).parsed
    val items = Def.setting(reductiveParser).parsed
    items foreach println
  }

  private def reductiveParser =
    ReductiveParser from Seq(
      "foo1.xml",
      "foo2.xml",
      "foo3.txt",
      "bar1.yml"
    )

  private def sampleParser =
    SampleParser selectSome Seq(
      "foo1.xml",
      "foo2.xml",
      "foo3.txt",
      "bar1.yml"
    )

  private def strictParser: Parser[Seq[String]] =
    StrictParser from Seq(
      "foo1.xml",
      "foo2.xml",
      "foo3.txt",
      "bar1.yml"
    )
}
