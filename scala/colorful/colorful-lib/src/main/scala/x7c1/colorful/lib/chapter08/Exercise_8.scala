package x7c1.colorful.lib.chapter08

import x7c1.colorful.lib.chapter06.RNG.nonNegativeInt
import x7c1.colorful.lib.chapter06.{RNG, SimpleRNG, State}


case class Gen[+A](sample: State[RNG, A]){

  /* 8.6 */

  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(sample flatMap { a => f(a).sample })
  }
  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap{ i => Gen.listOfN(i, this) }

  /* 8.10 */
  def unsized: SGen[A] = SGen(i => this)
}

object Gen {

  /* 8.4 */

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    val diff = stopExclusive - start
    val rng = RNG.map(nonNegativeInt){ i => (i % diff) + start }
    Gen[Int](State(rng))
  }

  /*
  // scrap, too slow :P
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    @tailrec
    def loop(range: Range)(rng: RNG): (Int, RNG) = {
      val (i, r) = rng.nextInt
      if (range contains i) (i, r)
      else loop(range)(r)
    }
    val range = start to stopExclusive dropRight 1
    Gen(State(loop(range)))
  }
  */

  /* 8.5 */

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(RNG.map(RNG.int){ _ % 2 == 0 }))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    val list = List.fill(n)(g.sample)
    Gen(State.sequence(list))
  }

  /* 8.7 */

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    boolean.flatMap(b => if(b) g1 else g2)
  }

  /* 8.8 */

  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
    val ((gen1, x1), (gen2, x2)) = (g1, g2)
    val p = x1 / (x1 + x2)
    val double = Gen(State(RNG.double))
    double.flatMap{ x => if (x < p) gen1 else gen2 }
  }

  /* 8.12 */

  def listOf[A](g: Gen[A]): SGen[List[A]] = {
    SGen(listOfN(_, g))
  }

  /* 8.1e */

  def listOf1[A](g: Gen[A]): SGen[List[A]] = {
    SGen(n => listOfN(n max 1, g))
  }
}

object Prop {
  type SuccessCount = Int
  type FailedCase = String
  type TestCases = Int
  type MaxSize = Int

  import x7c1.colorful.lib.chapter05.Stream

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g.forSize)(f)

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val evaluate = (a: A, i: Int) =>
        try if (f(a)) Passed else Falsified(a.toString, i)
        catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }

      randomStream(as)(rng).zip(Stream from 0).
        take(n).
        map(evaluate.tupled).
        find(_.isFalsified).getOrElse(Passed)
  }
  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n - 1) / max + 1
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, n, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def run(
      p: Prop,
      maxSize: Int = 100, testCases: Int = 100,
      rng: RNG = SimpleRNG(System.currentTimeMillis)): Unit =

    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Proved =>
        println(s"+ OK, proved property.")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
    }
}

import x7c1.colorful.lib.chapter08.Prop.{FailedCase, MaxSize, SuccessCount, TestCases}

case class Prop(run: (MaxSize, TestCases, RNG) => Result){
  def &&(p: Prop): Prop = Prop { (max, n, rng) =>
    run(max, n, rng) match {
      case f: Falsified => f
      case _ => p.run(max, n, rng)
    }
  }
  def ||(p: Prop): Prop = Prop { (max, n, rng) =>
    run(max, n, rng) match {
      case f: Falsified => p.run(max, n, rng)
      case x => x
    }
  }
}

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  override def isFalsified = false
}

case class Falsified(
  failure: FailedCase,
  successes: SuccessCount) extends Result {

  override def isFalsified = true
}

case object Proved extends Result {
  override def isFalsified: Boolean = false
}

case class SGen[+A](forSize: Int => Gen[A]){

  /* 8.11 */

  def flatMap[B](f: A => Gen[B]): SGen[B] = {
    SGen(i => forSize(i).flatMap(f))
  }

}

object Exercise_8_3 {
  trait Prop {
    def check: Boolean
    def &&(p: Prop): Prop = new Prop {
      override def check: Boolean = Prop.this.check && p.check
    }
  }
}

object Exercise_8_13 {

  import Gen.{listOf, listOf1}
  import Prop.forAll

  val smallInt = Gen.choose(-10, 10)

  val maxProp = forAll(listOf(smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }
  val maxProp1 = forAll(listOf1(smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }
  def main(args: Array[String]): Unit = {
    Prop.run(maxProp1)
  }
}

object Exercise_8_14 {

  import Gen.listOf1
  import Prop.forAll

  val smallInt = Gen.choose(-10, 10)

  val sortedProp = forAll(listOf1(smallInt)){ ns =>
    val list = ns.sorted
    list.zip(list drop 1).forall{ case (a, b) => a <= b }
  }
  def main(args: Array[String]): Unit = {
    Prop run sortedProp
  }
}
