package x7c1.colorful.lib.chapter12

import org.scalatest.{FlatSpecLike, Matchers}
import x7c1.colorful.lib.chapter10.{Monoid, Exercise_10_1}

import scala.language.higherKinds

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
import x7c1.colorful.lib.chapter10.Exercise_10_13.{Branch, Leaf, Tree => Tree0}

object treeTraverse extends Traverse[Tree0] {
  override def traverse[G[_] : Applicative, A, B]
  (fa: Tree0[A])(f: A => G[B]): G[Tree0[B]] = {

    val g = implicitly[Applicative[G]]
    fa match {
      case Leaf(a) => g.map(f(a)){Leaf(_)}
      case Branch(l, r) => g.map2(traverse(l)(f), traverse(r)(f)){ Branch(_, _) }
    }
  }
}

class Listing_12_14_Tests extends FlatSpecLike with Matchers {

  import Exercise_12_13.listTraverse

  "zipWithIndex" can "generate indexed list" in {
    val x = List("a", "b", "c")
    val y = listTraverse.zipWithIndex(x)
    y shouldBe List(("a", 0), ("b", 1), ("c", 2))
  }
  "zipWithIndex" can "generate indexed tree" in {
    val tree = Branch(
      Branch(Leaf("a"), Leaf("b")),
      Branch(Leaf("c"), Leaf("d"))
    )
    val y = treeTraverse.zipWithIndex(tree)
    y shouldBe Branch(
      Branch(Leaf(("a",0)), Leaf(("b",1))),
      Branch(Leaf(("c",2)), Leaf(("d",3)))
    )
  }
}

class Exercise_12_16_Tests extends FlatSpecLike with Matchers {

  import Exercise_12_13.listTraverse

  behavior of "reverse"
  it can "generate a list in the reverse order " in {
    val x = List("a", "b", "c")
    val y = listTraverse.reverse(x)
    y shouldBe List("c", "b", "a")
  }
  it can "generate a tree in the reverse order " in {
    val tree = Branch(
      Branch(Leaf("1"), Leaf("2")),
      Branch(Leaf("3"), Leaf("4"))
    )
    treeTraverse.reverse(tree) shouldBe Branch(
      Branch(Leaf("4"), Leaf("3")),
      Branch(Leaf("2"), Leaf("1"))
    )
  }

  behavior of "reverse-law"
  it should "hold on listTraverse" in {
    import listTraverse.{reverse, toList}
    val x = List("a", "b", "c")
    val y = List("d", "e", "f")

    toList(reverse(x)) ++ toList(reverse(y)) shouldBe
      reverse(toList(y) ++ toList(x))
  }
  it should "hold on treeTraverse" in {
    import treeTraverse.{reverse, toList}
    val x = Branch(
      Branch(Leaf("1"), Leaf("2")),
      Branch(Leaf("3"), Leaf("4"))
    )
    val y = Branch(
      Branch(Leaf("a"), Leaf("b")),
      Branch(Leaf("c"), Leaf("d"))
    )
    toList(reverse(x)) ++ toList(reverse(y)) shouldBe
      listTraverse.reverse(toList(y) ++ toList(x))
  }

}

class Exercise_12_17_Tests extends FlatSpecLike with Matchers {
  import Exercise_12_13.listTraverse

  "foldLeft" should "be implemented by mapAccum" in {
    val x = List("a", "b", "c")
    listTraverse.foldLeft(x)("!"){_ + _} shouldBe "!abc"
  }
}

class Exercise_12_19_Tests extends FlatSpecLike with Matchers {
  import Exercise_10_1.intAddition
  import Exercise_12_13.listTraverse

  "compose" can "combine two traversable functor" in {
    val x1 = Branch(
      Branch(Leaf("1"), Leaf("2")),
      Leaf("3")
    )
    val x2 = Branch(
      Leaf("4"),
      Branch(Leaf("5"), Leaf("6"))
    )
    val x3 = List(x1, x2)

    val traverse = listTraverse compose treeTraverse
    val y = traverse.foldLeft(x3)("!"){_ + _}
    y shouldBe "!123456"

    val y3 = traverse.foldMap(x3)(_.toInt)(intAddition: Monoid[Int])
    y3 shouldBe 21
  }
}
