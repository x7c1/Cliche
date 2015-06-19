package x7c1.colorful.lib.chapter08

import org.scalatest.{FlatSpecLike, Matchers}
import x7c1.colorful.lib.chapter06.Repeat

class Exercise_8_4_Test extends FlatSpecLike with Matchers {
  "choose" can "generate int values in target range" in {
    val (start, end) = (10, 100)
    val gen = Gen.choose(start, end)
    val list = Repeat(1000)(gen.sample.run)
    list foreach { i =>
      i shouldBe >=(start)
      i shouldBe <(end)
    }
  }
}

class Exercise_8_5_Test extends FlatSpecLike with Matchers {

  "unit" should "always generate given value" in {
    val original = 123
    val gen = Gen.unit(original)
    val list = Repeat(10)(gen.sample.run)
    list foreach { i =>
      i shouldBe original
    }
  }
  "boolean" should "generate bool value" in {
    val list = Repeat(5)(Gen.boolean.sample.run)
    list shouldBe List(true, false, false, false, true)
  }
  "listOfN" should "generate lists of length n" in {
    val (start, end) = (10, 100)
    val gen = Gen.choose(start, end)
    val expected = Repeat(10)(gen.sample.run).reverse
    val list = Repeat(1)(Gen.listOfN(10, gen).sample.run).flatten
    list shouldBe expected
  }
}

class Exercise_8_6_Test extends FlatSpecLike with Matchers {
  "listOfN" should "generate lists of length n" in {
    val (start, end) = (10, 100)
    val gen = Gen.choose(start, end)
    val expected = Repeat(10)(gen.sample.run).reverse
    val list = Repeat(1)(gen.listOfN(Gen.unit(10)).sample.run).flatten
    list shouldBe expected
  }
}

class Exercise_8_7_Test extends FlatSpecLike with Matchers {
  "union" should "generate value" in {
    val range1 = 1 to 10
    val gen1 = Gen.choose(range1.start, range1.end)

    val range2 = 101 to 200
    val gen2 = Gen.choose(range2.start, range2.end)

    val gen = Gen.union(gen1, gen2)
    val list = Repeat(100)(gen.sample.run)
    list foreach { n =>
      n should (be >= range1.start or be >= range2.start)
      n should (be < range1.end or be < range2.end)
    }
  }
}

class Exercise_8_8_Test extends FlatSpecLike with Matchers {
  "weighted" should "generate value with probability" in {
    val range1 = 1 to 10
    val gen1 = Gen.choose(range1.start, range1.end)

    val range2 = 101 to 200
    val gen2 = Gen.choose(range2.start, range2.end)

    val gen = Gen.weighted((gen1, 8), (gen2, 2))
    val list = Repeat(10000)(gen.sample.run)

    val (list1, list2) = list.partition(_ <= 10)
    list1.length shouldBe 8000
    list2.length shouldBe 2000
  }
}
