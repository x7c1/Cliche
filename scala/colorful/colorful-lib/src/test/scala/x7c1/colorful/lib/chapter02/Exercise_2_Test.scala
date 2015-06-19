package x7c1.colorful.lib.chapter02

import org.scalatest.{FunSpec, Matchers}
import x7c1.colorful.lib.chapter02.Exercise_2_1.fib
import x7c1.colorful.lib.chapter02.Exercise_2_2.{isSorted, isSorted2}
import x7c1.colorful.lib.chapter02.Exercise_2_3.curry
import x7c1.colorful.lib.chapter02.Exercise_2_4.uncurry
import x7c1.colorful.lib.chapter02.Exercise_2_5.compose

class Exercise_2_1_Test  extends FunSpec with Matchers {
  it("can call fib(n)"){
    (0 to 5) map fib shouldBe Seq(0, 1, 1, 2, 3, 5)
  }
}

class Exercise_2_2_Test  extends FunSpec with Matchers {
  val targets = {
    val f = (x: (Int, Int) => Boolean) => x
    Seq(
      (Array(),  f(_ > _), true),
      (Array(1), f(_ > _), true),
      (Array(4, 3, 2, 1, 0), f(_ > _), true),
      (Array(1, 2, 3, 4, 5), f(_ < _), true),
      (Array(3, 2, 3, 4, 1), f(_ < _), false),
      (Array(3, 2, 3, 4, 1), f(_ > _), false))
  }
  it("can call isSorted"){
    targets foreach { case (x, f, bool) => isSorted(x, f) shouldBe bool }
  }
  it("can call isSorted2"){
    targets foreach { case (x, f, bool) => isSorted2(x, f) shouldBe bool }
  }
}

class Exercise_2_3_Test  extends FunSpec with Matchers {
  it("can call curry"){
    val f: (Int, Int) => Int = (a, b) => a + b
    f(11, 22) shouldBe curry(f)(11)(22)
  }
}

class Exercise_2_4_Test  extends FunSpec with Matchers {
  it("can call uncurry"){
    val f: Int => Int => Int = a => b => a + b
    f(11)(22) shouldBe uncurry(f)(11, 22)
  }
}

class Exercise_2_5_Test  extends FunSpec with Matchers {
  it("can call compose"){
    val f = (_: Int) * 2
    val g = (_: Int) + 2
    compose(f, g)(3) shouldBe f(g(3))
  }
}
