package example

import x7c1.salad.inspector.{TypeDigest, TypeReflector}

object InspectorSample extends App {
  val digest = TypeReflector.inspect[SampleStructure]

  println(dump(digest))
  /*
  example.SampleStructure
    z : scala.collection.immutable.List[scala.Int]
    y : example.SampleGeneric[java.lang.String,scala.Long,scala.Int]
      c : example.Nested[example.Nested[example.Nested[scala.Int]]]
        foo : example.Nested[example.Nested[scala.Int]]
          foo : example.Nested[scala.Int]
            foo : scala.Int
      b : scala.Long
      a : java.lang.String
    x : scala.Int
   */

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
  def y: SampleGeneric[String, Long, Int]
  def z: List[Int]
}

trait SampleGeneric [A, B, C]{
  def a: A
  def b: B
  def c: Nested[Nested[Nested[C]]]
}

trait Nested[A]{
  def foo: A
}
