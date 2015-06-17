package x7c1.colorful.lib.chapter10

import fpinscala.monoids.Monoid
import org.scalatest.{FlatSpecLike, Matchers}

class Exercise_10_1_Test extends FlatSpecLike with Matchers {
  import Exercise_10_1._

  def testIntMonoid(m: Monoid[Int]) = {
    import m.{op, zero}
    val (x, y, z) = (11, 22, 33)
    op(op(x, y), z) shouldBe op(x, op(y, z))
    op(x, zero) shouldBe x
    op(zero, x) shouldBe x
  }
  "intAddition" should "be Monoid" in {
    testIntMonoid(intAddition)
  }
  "intMultiplication" should "be Monoid" in {
    testIntMonoid(intMultiplication)
  }

  def testBooleanMonoid(m: Monoid[Boolean]) = {
    import m.{op, zero}
    val (x, y, z) = (true, true, true)
    op(op(x, y), z) shouldBe op(x, op(y, z))
    op(x, zero) shouldBe x
    op(zero, x) shouldBe x
  }
  "booleanOr" should "be Monoid" in {
    testBooleanMonoid(booleanOr)
  }
  "booleanAnd" should "be Monoid" in {
    testBooleanMonoid(booleanAnd)
  }
}
