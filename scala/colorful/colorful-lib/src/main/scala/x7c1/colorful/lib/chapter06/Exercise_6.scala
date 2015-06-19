package x7c1.colorful.lib.chapter06

import scala.annotation.tailrec


trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  /* 6.1 */

  /*
    memo:
    scala> Int.MinValue
    res4: Int = -2147483648

    scala> -(Int.MinValue + 1)
    res5: Int = 2147483647

    scala> -(Int.MinValue)
    res6: Int = -2147483648
   */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    val next = if (i < 0) -(i + 1) else i
    next -> r
  }

  /* 6.2 */

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  /* 6.3 */

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    (i, d) -> r2
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, r1) = double(rng)
    val (i, r2) = r1.nextInt
    (d, i) -> r2
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    (d1, d2, d3) -> r3
  }

  /* 6.4 */

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def loop(n: Int, rng: RNG, result: List[Int]): (List[Int], RNG) =
      n match {
        case _ if n <= 0 => (result, rng)
        case _ =>
          val (i, r) = rng.nextInt
          loop(n - 1, r, i :: result)
      }

    loop(count, rng, List())
  }

  type Rand[+A] = RNG => (A, RNG)

  /* // after Exercise 6.9
  type State[S,+A] = S => (A,S)
  type Rand[A] = State[RNG, A]
   */

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  /* 6.5 */

  def double2: Rand[Double] = {
    map(nonNegativeInt) { _ / (Int.MaxValue.toDouble + 1) }
  }

  /* 6.6 */

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng =>
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      f(a, b) -> r2
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  /* 6.7 */

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    rng =>
      fs.foldRight(List[A]() -> rng) {
        case (rand, (list, r)) =>
          val (a, next) = rand(r)
          (a :: list) -> next
      }
  }

  def ints2(count: Int): Rand[List[Int]] = {
    sequence(List.fill(count)(int))
  }

  /* 6.8 */

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng =>
      val (i, r) = f(rng)
      g(i)(r)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod)
      else nonNegativeLessThan(n)
    }

  /* 6.9 */

  def mapNew[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s) { a => unit(f(a)) }
  }

  def map2New[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra) { a =>
      flatMap(rb) { b => unit(f(a, b)) }
    }
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object Repeat {
  def apply[A](count: Int)(f: RNG => (A, RNG)): List[A] = {
    new Repeat(f).loop(count, SimpleRNG(1))
  }
}
class Repeat[A](f: RNG => (A, RNG)){
  @tailrec
  final def loop(count: Int, rng: RNG, list: List[A] = List()): List[A] =
    count match {
      case _ if count > 0 =>
        val (i, r) = f(rng)
        loop(count - 1, r, i :: list)
      case _ => list
    }
}

/* 6.10 */

object State {

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
    val init = unit[S, List[A]](List())
    fs.foldRight(init){(s, acc) => s.map2(acc){_ :: _} }
  }
  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}

case class State[S, +A](run: S => (A, S)){

  def map[B](f: A => B): State[S, B] = {
    flatMap(f andThen State.unit)
  }
  def map2[B, C](rb: State[S, B])(f: (A, B) => C): State[S, C] = {
    flatMap(a => rb map { b => f(a, b) })
  }
  def flatMap[B](g: A => State[S, B]): State[S, B] = State {
    s =>
      val (a, s2) = run(s)
      g(a).run(s2)
  }
}

/* 6.11 */

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Exercise_6_1 {
  def main(args: Array[String]) = {
    val list = Repeat(10)(RNG.nonNegativeInt)
    list foreach println
  }
}

object Exercise_6_2 {
  def main(args: Array[String]) = {
    val list = Repeat(10)(RNG.double)
    list foreach println
  }
}

object Exercise_6_3 {
  def main(args: Array[String]) = {
    Repeat(10)(RNG.intDouble) foreach println
    Repeat(10)(RNG.doubleInt) foreach println
    Repeat(10)(RNG.double3) foreach println
  }
}

object Exercise_6_4 {
  def main(args: Array[String]) = {
    val (list, rng) = RNG.ints(20)(SimpleRNG(1))
    list foreach println
  }
}

object Exercise_6_5 {
  def main(args: Array[String]) = {
    val list = Repeat(10)(RNG.double2)
    list foreach println
  }
}

object Exercise_6_6 {
  def main(args: Array[String]) = {
    Repeat(10)(RNG.randIntDouble) foreach println
    Repeat(10)(RNG.randDoubleInt) foreach println
  }
}

object Exercise_6_7 {
  def main(args: Array[String]) = {
    val (list, rng) = RNG.ints2(20)(SimpleRNG(1))
    list foreach println
  }
}

object Exercise_6_8 {
  def main(args: Array[String]) = {
    Repeat(10)(RNG.nonNegativeLessThan(1000)) foreach println
  }
}

object Exercise_6_11 {
  import State.{modify, sequence, get}

  def update = (input: Input) => (machine: Machine) => input match {
    case Coin if machine.candies > 0 && machine.locked =>
      machine.copy(locked = false, coins = machine.coins + 1)
    case Turn if ! machine.locked =>
      machine.copy(locked = true, candies = machine.candies - 1)
    case _ =>
      machine
  }
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      _ <- sequence(inputs map (modify[Machine] _ compose update))
      machine <- get
    } yield {
      machine.coins -> machine.candies
    }

}
