package x7c1.colorful.lib.chapter15

import org.scalatest.{FlatSpecLike, Matchers}

class Exercise_15_1_Tests extends FlatSpecLike with Matchers {
  import Process.{drop, dropWhile, take, takeWhile}

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

class Exercise_15_2_Tests extends FlatSpecLike with Matchers {
  import Process.count

  "count" should "emit the number of elements" in {
    count(Stream("a","b","c")) shouldBe Stream(1,2,3)
  }
}

class Exercise_15_3_Tests extends FlatSpecLike with Matchers {
  import Process.mean

  "mean" should "emit the number of elements" in {
    mean(Stream(1.5,2.5,3.5)) shouldBe Stream(1.5,2.0,2.5)
  }
}

class Exercise_15_4_Tests extends FlatSpecLike with Matchers {
  import Process.count2

  "count2" should "emit the number of elements" in {
    count2(Stream("a","b","c")) shouldBe Stream(1,2,3)
  }
}

class Exercise_15_5_Tests extends FlatSpecLike with Matchers {
  import Process.{filter, lift}

  "|>" should "emit the number of elements" in {
    val x = Stream(1, 2, 3, 4, 5, 6)

    val p1 = filter[Int](_ % 2 == 0) |> lift(_ + 10)
    p1(x) shouldBe Stream(12,14,16)

    val p2 = p1 |> lift(_ * 10)
    p2(x) shouldBe Stream(120,140,160)

    val p3 = p2 |> filter(_ > 150)
    p3(x) shouldBe Stream(160)

  }
}
