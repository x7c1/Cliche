package x7c1.colorful.lib.chapter06

import org.scalatest.{FlatSpecLike, Matchers}

class Exercise_6_9_Tests extends FlatSpecLike with Matchers {

  "mapNew" can "behave same as map" in {
    val f: Int => Double = _ / (Int.MaxValue.toDouble + 1)
    val s1 = Repeat(10)(RNG.mapNew(RNG.nonNegativeInt)(f))
    val s2 = Repeat(10)(RNG.map(RNG.nonNegativeInt)(f))
    s1 shouldBe s2
  }

  "map2New" can "behave same as map2" in {
    import RNG.{Rand, map2, map2New, int, double}

    type MAP2[A, B] = (Rand[A], Rand[B]) => ((A, B) => (A, B)) => Rand[(A, B)]
    def f[A, B](x: Rand[A], y: Rand[B]) = (f: MAP2[A, B]) => f(x, y)((_, _))

    def intDoubleBy = f(int, double)
    val s1 = Repeat(10)(intDoubleBy(map2))
    val s2 = Repeat(10)(intDoubleBy(map2New))
    s1 shouldBe s2

    def doubleIntBy = f(double, int)
    val s3 = Repeat(10)(intDoubleBy(map2))
    val s4 = Repeat(10)(intDoubleBy(map2New))
    s3 shouldBe s4
  }
}

class Exercise_6_10_Tests extends FlatSpecLike with Matchers {

  case class Container[A](value: A)

  val multiply2 = State[Container[Int], Int] { container =>
    val next = container.value * 2
    (next, container.copy(next))
  }
  val plus1 = State[Container[Int], Int] { container =>
    val next = container.value + 1
    (next, container.copy(next))
  }

  behavior of "State"

  it should "have unit" in {
    val s = State.unit[Container[Int], Int](123)
    s.run(Container(1)) shouldBe (123, Container(1))
  }
  it should "have map" in {
    val s1 = multiply2.map(_ + 1)
    s1.run(Container(3)) shouldBe (7, Container(6))
  }
  it should "have map2" in {
    val s1 = multiply2.map2(plus1){ (i, s) => s"$i,$s" }
    s1.run(Container(3)) shouldBe ("6,7", Container(7))

    val s2 = multiply2.map2(plus1){ (i, s) => (i, s) }
    s2.run(Container(3)) shouldBe ((6, 7), Container(7))
  }
  it should "have flatMap" in {
    val s1 = multiply2.flatMap(i => State.unit(i + 1))
    s1.run(Container(3)) shouldBe (7, Container(6))
  }
  it should "have sequence" in {
    val s1 = State sequence List.fill(3)(multiply2)
    s1.run(Container(2)) shouldBe (List(4, 8, 16), Container(16))

    val s2 = multiply2.map2(plus1){ (_, _) }
    val s3 = State sequence List.fill(3)(s2)
    s3.run(Container(2)) shouldBe (List((4, 5), (10, 11), (22, 23)), Container(23))
  }
}

class Exercise_6_11_Tests extends FlatSpecLike with Matchers {
  import Exercise_6_11.simulateMachine

  val original = Machine(
    locked = true,
    candies = 5,
    coins = 10
  )
  behavior of "simulateMachine"

  it can "return resources in machine" in {
    val inputs = List(
      Coin, Turn,
      Coin, Turn,
      Coin, Turn,
      Coin, Turn
    )
    val ((coins, candies), _) = simulateMachine(inputs) run original
    (coins, candies) shouldBe (14, 1)
  }
  it should "ignore any inputs if locked" in {
    val inputs = List(Turn, Turn, Turn)
    val ((coins, candies), machine) = simulateMachine(inputs) run original
    (coins, candies) shouldBe (original.coins, original.candies)
    machine shouldBe original
  }
  it should "ignore any coins if unlocked" in {
    val inputs = List(Coin, Coin, Coin, Turn)
    val ((coins, candies), _) = simulateMachine(inputs) run original
    (coins, candies) shouldBe (11, 4)
  }
  it should "ignore any inputs if sold out" in {
    val inputs = List.fill(5)(List(Coin, Turn)).flatten
    val ((coins1, candies1), exhausted) = simulateMachine(inputs) run original
    (coins1, candies1) shouldBe (15, 0)

    val ((coins2, candies2), machine) = simulateMachine(inputs) run exhausted
    (coins2, candies2) shouldBe (exhausted.coins, exhausted.candies)
    machine shouldBe exhausted
  }
}
