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

  "mean" should "emit a running average of the values" in {
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

  "|>" should "fuse the transformations done by left and right" in {
    val x = Stream(1, 2, 3, 4, 5, 6)

    val p1 = filter[Int](_ % 2 == 0) |> lift(_ + 10)
    p1(x) shouldBe Stream(12,14,16)

    val p2 = p1 |> lift(_ * 10)
    p2(x) shouldBe Stream(120,140,160)

    val p3 = p2 |> filter(_ > 150)
    p3(x) shouldBe Stream(160)

  }
}

class Exercise_15_6_Tests extends FlatSpecLike with Matchers {
  import Process.drop

  "zipWithIndex" should "emit a running count of values" in {
    val x = Stream("a","b","c")

    val s1 = drop(0)(x)
    s1.toList shouldBe List("a","b","c")

    val s2 = drop(0).zipWithIndex(x)
    s2.toList shouldBe List(
      ("a",0),
      ("b",1),
      ("c",2)
    )
  }
}

class Exercise_15_7_Tests extends FlatSpecLike with Matchers {
  import Process.mean2

  "mean2" should "behave same as mean" in {
    mean2(Stream(1.5,2.5,3.5)) shouldBe Stream(1.5,2.0,2.5)
  }
}

class Exercise_15_8_Tests extends FlatSpecLike with Matchers {
  import Process.exists

  "exists" should "" in {
    exists[Int](_ % 2 == 0)(Stream(1,3,5,6,7)) shouldBe Stream(true)
    exists[Int](_ % 2 == 0)(Stream(1,3,5,7)) shouldBe Stream(false)
  }
}

class Exercise_15_9_Tests extends FlatSpecLike with Matchers {
  import Exercise_15_9.runToCelsius

  "runToCelsius" should "" in {
    val before = MockBuffer(Seq("140.0", "#comment", "149.0"))
    val after = runToCelsius(before)

    after.logs shouldBe Vector(
      "open to read fahrenheit.txt",
      "open to write celsius.txt",
      "read lines",
      "write line 60.0",
      "write line 65.0",
      "close file MockHandlerToWrite(celsius.txt)",
      "close file MockHandlerToRead(fahrenheit.txt)"
    )
    after.closed shouldBe Vector(
      MockHandlerToWrite("celsius.txt"),
      MockHandlerToRead("fahrenheit.txt")
    )
  }
}
