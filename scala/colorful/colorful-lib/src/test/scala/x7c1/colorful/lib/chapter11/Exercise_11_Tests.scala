package x7c1.colorful.lib.chapter11

import org.scalatest.{FlatSpecLike, Matchers}

class Exercise_11_3_Tests  extends FlatSpecLike with Matchers {
  import Exercise_11_1.optionMonad

  "monad" should "have sequence" in {
    val x = List(Some(1), Some(2), Some(3))
    optionMonad.sequence(x) shouldBe Some(List(1, 2, 3))

    val x2 = List(Some(1), None, Some(3))
    optionMonad.sequence(x2) shouldBe None
  }

  "monad" should "have traverse" in {
    val x = List(1, 2)
    val f: Int => Option[String] = i => Some((i * 2).toString)
    optionMonad.traverse(x)(f) shouldBe Some(List("2", "4"))

    val f2: Int => Option[String] = i => if (i % 2 == 0) Some(i.toString) else None
    optionMonad.traverse(x){f2} shouldBe None
  }
}

class Exercise_11_4_Tests  extends FlatSpecLike with Matchers {
  import Exercise_11_1.{listMonad, optionMonad}

  "listMonad" should "have replicateM" in {
    /*
    listMonad.map2(List(1, 2), List(List())){_ :: _}
      => L(L(1), L(2))
    listMonad.map2(List(1, 2), List(List(1), List(2))){_ :: _}
      => L(L(1,1), L(1,2), L(2,1), L(2,2))
    listMonad.map2(List(1, 2), List(List(1, 1), List(1, 2), List(2, 1), List(2, 2))){_ :: _}
      => L(L(1,1,1), L(1,1,2), L(1,2,1), L(1,2,2), ...)
     */
    listMonad.replicateM(3, List(1, 2)) shouldBe List(
      List(1, 1, 1), List(1, 1, 2), List(1, 2, 1), List(1, 2, 2),
      List(2, 1, 1), List(2, 1, 2), List(2, 2, 1), List(2, 2, 2))

    listMonad.replicateM(3, List(1)) shouldBe List(List(1, 1, 1))

    /*
    listMonad.map2(List(1,2,3), List(List())){_ :: _}
      => L(L(1), L(2), L(3))
    listMonad.map2(List(1,2,3), List(List(1), List(2), List(3))){_ :: _} =>
      => L(L(1,1), L(1,2), L(1,3), L(2,1), L(2,2), L(2,3), L(3,1), L(3,2), L(3,3))
     */
    listMonad.replicateM(2, List(1, 2, 3)) shouldBe List(
      List(1, 1), List(1, 2), List(1, 3),
      List(2, 1), List(2, 2), List(2, 3),
      List(3, 1), List(3, 2), List(3, 3)
    )
  }
  "optionMonad" should "have replicateM" in {
    /*
    optionMonad.map2(Some(1), Some(List())){_ :: _}
      => Some(L(1))
    optionMonad.map2(Some(1), Some(List(1))){_ :: _}
      => Some(L(1,1))
    optionMonad.map2(Some(1), Some(List(1,1))){_ :: _}
      => Some(L(1,1,1))
     */
    optionMonad.replicateM(3, Some(1)) shouldBe Some(List(1, 1, 1))
    optionMonad.replicateM(0, Some(1)) shouldBe Some(List())
    optionMonad.replicateM(3, None) shouldBe None
  }

}

class Exercise_11_6_Tests  extends FlatSpecLike with Matchers {
  import Exercise_11_1.{listMonad, optionMonad}

  "listMonad" should "have filterM" in {
    listMonad.filterM(List(1, 2, 3)){i => List(i % 2 == 0) } shouldBe List(List(2))
  }
  "optionMonad" should "have filterM" in {
    type F = Int => Option[Boolean]

    val f1 : F = i => Some(i % 2 == 0)
    optionMonad.filterM(List(1, 2, 3))(f1) shouldBe Some(List(2))

    val f2: F = i => if (i % 2 == 0) Some(true) else None
    optionMonad.filterM(List(1, 3))(f2) shouldBe None
  }
}

class Exercise_11_7_Tests  extends FlatSpecLike with Matchers {
  import Exercise_11_1.listMonad

  "listMonad" should "have compose" in {
    val f = (s: String) => List(s.toInt)
    val g = (i: Int) => List((i * 10).toDouble -> (i * 100).toDouble)
    val h = listMonad.compose(f, g)
    h("2") shouldBe List(20.0 -> 200.0)
  }
}

class Exercise_11_8_Tests  extends FlatSpecLike with Matchers {
  import Exercise_11_1.listMonad

  "listMonad" should "have flatMap_byCompose" in {
    val f = (s: String) => List(s.toInt)
    val ma = List("10", "100")
    listMonad.flatMap_byCompose(ma)(f) shouldBe List(10, 100)
  }
  "listMonad" should "have flatMap_byCompose_4" in {
    val f = (s: String) => List(s.toInt)
    val ma = List("10", "100")
    listMonad.flatMap_byCompose_4(ma)(f) shouldBe List(10, 100)
  }
}
