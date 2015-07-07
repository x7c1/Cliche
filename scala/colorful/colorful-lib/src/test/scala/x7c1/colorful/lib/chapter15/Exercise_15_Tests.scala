package x7c1.colorful.lib.chapter15

import org.scalatest.{Matchers, FlatSpecLike}

class Exercise_15_Tests extends FlatSpecLike with Matchers {
  import Process.{take, drop, takeWhile, dropWhile}

  "take" should "halt after encountering the given number of elements" in {
    take(3)(Stream(1,2,3,4,5)).toList shouldBe List(1,2,3)
  }
  "drop" should "ignore the given number of elements" in {
    drop(3)(Stream(1,2,3,4,5)).toList shouldBe List(4,5)
  }
  "takeWhile" should "take elements while the given predicate remains true" in {
    takeWhile((_: Int) < 3)(Stream(1,2,3,4,5)).toList shouldBe List(1,2)
  }
  "dropWhile" should "drop elements while the given predicate remains true" in {
    dropWhile((_: Int) < 3)(Stream(1,2,3,4,5)).toList shouldBe List(3,4,5)
  }
}
