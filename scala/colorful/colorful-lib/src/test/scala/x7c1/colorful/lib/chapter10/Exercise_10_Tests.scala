package x7c1.colorful.lib.chapter10

import java.util.concurrent.Executors

import org.scalatest.exceptions.TestFailedException
import org.scalatest.{FlatSpecLike, Matchers}
import x7c1.colorful.lib.chapter06.SimpleRNG
import x7c1.colorful.lib.chapter08.{Falsified, Gen, Prop}
import x7c1.colorful.lib.chapter10.Exercise_10_4.monoidLaws

object PropTestHelpers {
  def runProp(prop: Prop): Unit = {
    val (maxSize, testCases, rng) = (100, 100, SimpleRNG(1))
    prop.run(maxSize, testCases, rng) match {
      case Falsified(message, count) =>
        val line = s"! Falsified after $count passed tests:\n $message"
        throw new TestFailedException(line, 0)
      case _ =>
    }
  }
}

class Exercise_10_1_Test extends FlatSpecLike with Matchers {
  import Exercise_10_1._
  import Exercise_10_4.monoidLaws
  import PropTestHelpers.runProp

  def intMonoidProp(m: Monoid[Int]) = {
    val domain = Gen.choose(-100, 100)
    monoidLaws(m, domain)
  }
  def booleanMonoidProp(m: Monoid[Boolean]) = {
    val domain = Gen.boolean
    monoidLaws(m, domain)
  }
  "intAddition" should "be Monoid" in {
    runProp(intMonoidProp(intAddition))
  }
  "intMultiplication" should "be Monoid" in {
    runProp(intMonoidProp(intMultiplication))
  }
  "booleanOr" should "be Monoid" in {
    runProp(booleanMonoidProp(booleanOr))
  }
  "booleanAnd" should "be Monoid" in {
    runProp(booleanMonoidProp(booleanAnd))
  }
}

class Exercise_10_2_Test extends FlatSpecLike with Matchers {
  import Exercise_10_2.optionMonoid
  import PropTestHelpers.runProp

  def optionMonoidProp(m: Monoid[Option[Int]]): Prop = {
    val domain = Gen.boolean.map {
      case true => Some(1)
      case false => None
    }
    monoidLaws(m, domain)
  }
  "booleanAnd" should "be Monoid" in {
    runProp(optionMonoidProp(optionMonoid[Int]))
  }
}

class Exercise_10_3_Test extends FlatSpecLike with Matchers {
  import Exercise_10_3.endoMonoid
  import PropTestHelpers.runProp
  import x7c1.colorful.lib.chapter06.State

  type Endo[A] = A => A

  def isIntEndoEqual(f: Endo[Int], g: Endo[Int]): Boolean = {
    val (maxSize, testCases, rng) = (100, 100, SimpleRNG(1))
    val prop = Prop.forAll(Gen.choose(-100, 100)){ a => f(a) == g(a) }
    ! prop.run(maxSize, testCases, rng).isFalsified
  }

  def intEndoMonoidProp(m: Monoid[Endo[Int]]): Prop = {
    val domain = {
      val endo = (f: Endo[Int]) => Gen(State{ rng => (f, rng) })
      val d1 = endo(_ * 11)
      val d2 = endo(_ + 22)
      val d3 = endo(_ / 33)
      Gen.union(Gen.union(d1, d2), d3)
    }
    val monoid = endoMonoid[Int]
    import monoid.{op, zero}

    Prop.forAll(Gen.listOfN(3, domain)){
      case List(x, y, z) =>
        val associativity =  {
          val (f, g) = (op(op(x, y), z), op(x, op(y, z)))
          isIntEndoEqual(f, g)
        }
        val identity = {
          isIntEndoEqual(op(x, zero), x) &&
          isIntEndoEqual(op(zero, x), x)
        }
        associativity && identity
    }
  }

  "endoMonoid" should "be Monoid" in {
    runProp(intEndoMonoidProp(endoMonoid[Int]))
  }
}

class Exercise_10_5_Test extends FlatSpecLike with Matchers {
  import Exercise_10_5.stringMonoid

  "foldMap" can "fold map & fold" in {
    val words = List(1, 2, 3)
    Monoid.foldMap(words, stringMonoid){_.toString} shouldBe "123"
    Monoid.foldMap2(words, stringMonoid){_.toString} shouldBe "123"
  }
}

class Exercise_10_6_Test extends FlatSpecLike with Matchers {
  import Exercise_10_6.{foldLeft, foldLeft2, foldRight, foldRight2}

  "foldRight" can "fold right-to-left" in {
    val (z, list) = "!" -> List("a", "b", "c")
    foldRight(list, z){_ + _} shouldBe list.foldRight(z){_ + _}
    foldRight2(list, z){_ + _} shouldBe list.foldRight(z){_ + _}
  }
  "foldLeft" can "fold left-to-right" in {
    val (z, list) = "!" -> List("a", "b", "c")
    foldLeft(list, z){_ + _} shouldBe list.foldLeft(z){_ + _}
    foldLeft2(list, z){_ + _} shouldBe list.foldLeft(z){_ + _}
  }
}

class Exercise_10_7_Test extends FlatSpecLike with Matchers {
  import Exercise_10_5.stringMonoid

  "foldMapV" can "behave same as foldMap" in {
    val words = IndexedSeq(1, 2, 3)
    Monoid.foldMapV(words, stringMonoid){_.toString} shouldBe "123"

    val words1 = IndexedSeq(1)
    Monoid.foldMapV(words1, stringMonoid){_.toString} shouldBe "1"

    val words2 = IndexedSeq(1, 2)
    Monoid.foldMapV(words2, stringMonoid){_.toString} shouldBe "12"

    val empty = IndexedSeq()
    Monoid.foldMapV(empty, stringMonoid){_.toString} shouldBe ""
  }
}

class Exercise_10_8_Test extends FlatSpecLike with Matchers {
  import Exercise_10_5.stringMonoid
  import Exercise_10_8.{parFoldMap, parFoldMap2, parFoldMap3}
  import fpinscala.parallelism.Nonblocking.Par

  "parFoldMap" can "behave same as foldMapV" in {
    val words = IndexedSeq(1, 2, 3)
    val service = Executors.newFixedThreadPool(10)

    Par.run(service)(parFoldMap(words,  stringMonoid){_.toString}) shouldBe "123"
    Par.run(service)(parFoldMap2(words, stringMonoid){_.toString}) shouldBe "123"
    Par.run(service)(parFoldMap3(words, stringMonoid){_.toString}) shouldBe "123"
  }
}

class Exercise_10_9_Test extends FlatSpecLike with Matchers {
  import Exercise_10_4.monoidLaws
  import Exercise_10_9.{isSorted, Piece, comparatorMonoid, isAscendingOrdered, isDescendingOrdered}
  import PropTestHelpers.runProp

  def pieceProp(m: Monoid[Piece]) = {
    val domain = Gen.choose(-100, 100).map2(Gen.boolean){
      (i, b) => Some((i, i, b))
    }
    monoidLaws(m, domain)
  }
  "comparatorMonoid" can "be Monoid" in {
    runProp(pieceProp(comparatorMonoid))
  }
  "isSorted" can "detect whether list is ordered" in {
    isSorted(IndexedSeq(0, 1, 2)) shouldBe true
  }
  "isAscendingOrdered" can "detect whether list is ascending-ordered" in {
    isAscendingOrdered(IndexedSeq(0, 1, 2)) shouldBe true
    isAscendingOrdered(IndexedSeq(3, 2, 1)) shouldBe false
  }
  "isDescendingOrdered" can "detect whether list is descending-ordered" in {
    isDescendingOrdered(IndexedSeq(3, 2, 1)) shouldBe true
    isDescendingOrdered(IndexedSeq(0, 1, 2)) shouldBe false
  }
}
