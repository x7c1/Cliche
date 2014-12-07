package sample

import x7c1.salad.inspector.{TypeDigest, TypeReflector}

object InspectorSample extends App {
  val digest = TypeReflector.inspect[SampleStructure]

  println(dump(digest))

  def dump(digest: TypeDigest, indent: Int = 1): String = {
    val lines = digest.typedName +:
      digest.members.
        map { f => s"${f.decodedName} : ${dump(f.resultType, indent + 1)}" }.
        map { "  " * indent + _ }

    lines.mkString("\n")
  }
}

trait SampleStructure {
  def x: Int
  def y: SampleValues[String, Long, Int]
  def z: List[Int]
}

trait SampleValues [A, B, C]{
  def a: A
  def b: B
  def c: Nested[Nested[Nested[C]]]
}

trait Nested[A]{
  def foo: A
}
