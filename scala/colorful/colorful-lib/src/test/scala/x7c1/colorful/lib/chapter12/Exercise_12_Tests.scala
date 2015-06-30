package x7c1.colorful.lib.chapter12

import org.scalatest.{FlatSpecLike, Matchers}

class Exercise_12_4_Tests extends FlatSpecLike with Matchers {
  import Exercise_12_4.streamApplicative

  "sequence(streams)" can "slice each stream and generate lists of these elements" in {
    val x1 = List(
      Stream(1, 2, 3),
      Stream(10, 20, 30)
    )
    val s1 = streamApplicative.sequence(x1)
    s1 shouldBe Stream(
      List(1, 10),
      List(2, 20),
      List(3, 30)
    )
    val x2 = List(
      (1 to 1000).toStream,
      (100 to 200).toStream
    )
    val s2 = streamApplicative.sequence(x2)
    intercept[IndexOutOfBoundsException]{ s2(101) }

    lazy val fibs: Stream[Int] = Stream.cons(0, fibs.scanLeft(1)(_ + _))
    val x3 = List(fibs, fibs.tail)
    val s3 = streamApplicative.sequence(x3)
    s3.slice(0, 7) shouldBe Stream(
      List(0, 1),
      List(1, 1),
      List(1, 2),
      List(2, 3),
      List(3, 5),
      List(5, 8),
      List(8, 13)
    )
  }
}

class Exercise_12_5_Tests extends FlatSpecLike with Matchers {

}
