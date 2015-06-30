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

class Exercise_11_18_Tests  extends FlatSpecLike with Matchers {
  import Monad.stateMonad
  import x7c1.colorful.lib.chapter06.Exercise_6_11.simulateMachine
  import x7c1.colorful.lib.chapter06.{Coin, Input, Machine, State, Turn}

  def replicateM[S, A] = stateMonad[S].replicateM[A] _
  def sequence[S, A] = stateMonad[S].sequence[A] _

  val original = Machine(
    locked = true,
    candies = 5,
    coins = 10
  )
  def s1(inputs: List[Input]) = simulateMachine(inputs)

  "replicateM(n, s1)" can "illustrate a process of n-times transition from s1" in {
    val inputs = List(
      Coin, Turn
    )
    val (process, updatedMachine) = replicateM(4, s1(inputs)) run original

    process shouldBe List((11,4), (12,3), (13,2), (14,1))
    updatedMachine shouldBe Machine(locked = true, candies = 1, coins = 14)
  }
  "map2" can "merge two state" in {
    val inputs = List(
      Coin, Turn,
      Coin, Turn,
      Coin, Turn,
      Coin, Turn
    )
    val getLocked: State[Machine, Boolean] = State.get[Machine].map(_.locked)
    val s2 = stateMonad[Machine].map2(s1(inputs), getLocked){
      (y, locked_) => (y._1, y._2, locked_)
    }
    val ((coins, candies, locked), updatedMachine) = s2 run original

    coins shouldBe 14
    candies shouldBe 1
    locked shouldBe true

    updatedMachine shouldBe Machine(
      locked = true,
      candies = 1,
      coins = 14)
  }
  "for-yield" can "generate a new state like map2" in {
    val inputs = List(
      Coin, Turn,
      Coin, Turn,
      Coin, Turn,
      Coin, Turn
    )
    val s2 = for {
      y <- s1(inputs)
      z <- State.get[Machine]
    } yield {
      (y._1, y._2, z.locked)
    }
    val ((coins, candies, locked), updatedMachine) = s2 run original

    coins shouldBe 14
    candies shouldBe 1
    locked shouldBe true

    updatedMachine shouldBe Machine(
      locked = true,
      candies = 1,
      coins = 14)
  }

  "sequence" can "illustrate a process of transition from given states" in {
    val inputs = List(
      Coin, Turn
    )
    val states = List.fill(4)(s1(inputs))
    val s2: State[Machine, List[(Int, Int)]] = sequence(states)

    val (process, updatedMachine) = s2 run original
    process shouldBe List((11,4), (12,3), (13,2), (14,1))
    updatedMachine shouldBe Machine(locked = true, candies = 1, coins = 14)
  }
}

class Exercise_11_19_Tests  extends FlatSpecLike with Matchers {

  import Monad.stateMonad
  import Exercise_11_19.getState
  import x7c1.colorful.lib.chapter08.{Gen, Prop}
  import x7c1.colorful.lib.chapter10.PropTestHelpers.runProp
  import x7c1.colorful.lib.chapter06.State

  def intProp[A](f: Int => Boolean) =
    Prop.forAll(Gen.choose(-100, 100))(f)

  /* based on answers */

  "getState.flatMap(setState)" should "be unit(())" in {

    def test[S](s: S) = {
      def setState(s: S): State[S, Unit] = Exercise_11_19.setState(s)

      val s1: State[S, Unit] = getState flatMap setState
      val s2: State[S, Unit] = stateMonad unit {()}
      s1.run(s) == s2.run(s)
    }
    def testByFor[S](s: S) = {
      import Exercise_11_19.setState

      val s1: State[S, Unit] = for{
        x <- getState
        _ <- setState(x)
      } yield ()

      val s2: State[S, Unit] = stateMonad unit {()}
      s1.run(s) == s2.run(s)
    }
    runProp(intProp(test))
    runProp(intProp(testByFor))
  }
  "setState(x).flatMap(_ => getState)" should "be unit(x)" in {

    def test[S](x: S) = {
      import Exercise_11_19.setState

      val s1: State[S, S] = setState(x) flatMap {_ => getState}
      val s2: State[S, S] = stateMonad unit x
      s1.run(x) == s2.run(x)
    }
    def testByFor[S](x: S) = {
      import Exercise_11_19.setState

      val s1: State[S, S] = for{
        _ <- setState(x)
        x <- getState
      } yield x

      val s2: State[S, S] = stateMonad unit x
      s1.run(x) == s2.run(x)
    }
    runProp(intProp(test))
    runProp(intProp(testByFor))
  }
}

class Exercise_11_20_Tests extends FlatSpecLike with Matchers {

  import Reader.readerMonad
  case class Context(x1: Int, x2: String)

  "sequence" can "generate new List of the results from readers'.run" in {
    val y1 = Reader[Context, Int](_.x1)
    val y2 = Reader[Context, Int](_.x1 * 2)
    val c = Context(123, "hello")

    readerMonad.sequence(List(y1, y2)).run(c) shouldBe List(123, 246)
  }

  "join" can "unwrap Reader(Reader(f)) to Reader(f)" in {
    val y1 = Reader[Context, Int](_.x1 * 2)
    val y2 = Reader[Context, Reader[Context, Int]](_ => y1)
    val c = Context(123, "hello")

    readerMonad.join(y2).run(c) shouldBe 246
  }

  "replicateM(n, r1)" can "repeat a function in r1 n-times" in {
    val y1 = Reader[Context, Int](_.x1 * 2)
    val c = Context(123, "hello")

    readerMonad.replicateM(3, y1).run(c) shouldBe List(246, 246, 246)
  }

}
