package x7c1.colorful.lib.chapter14

/* Listing 14-2 */
sealed trait ST[S,A] { self =>

  protected def run(s: S): (A,S)

  def map[B](f: A => B): ST[S,B] = new ST[S,B] {
    def run(s: S) = {
      val (a, s1) = self.run(s)
      (f(a), s1) }
  }
  def flatMap[B](f: A => ST[S,B]): ST[S,B] = new ST[S,B] {
    def run(s: S) = {
      val (a, s1) = self.run(s)
      f(a).run(s1)
    }
  }
}

/* Listing 14-3 */
sealed trait STRef[S,A] {
  protected var cell: A
  def read: ST[S,A] = ST(cell)
  def write(a: A): ST[S,Unit] = new ST[S,Unit] {
    def run(s: S) = {
      cell = a
      ((), s)
    }
  }
}
object STRef {
  def apply[S,A](a: A): ST[S, STRef[S,A]] = ST(new STRef[S,A] {
    var cell = a
  })
}

/* Listing 14-4 */
trait RunnableST[A] {
  def apply[S]: ST[S,A]
}

/* Listing 14-5 */
object ST {
  def apply[S,A](a: => A): ST[S, A] = {
    lazy val memo = a
    new ST[S,A] {
      override def run(s: S) = (memo, s)
    }
  }
  def runST[A](st: RunnableST[A]): A =
    st.apply[Unit].run(())._1
}

/* Listing 14-6 */
sealed abstract class STArray[S,A](implicit manifest: Manifest[A]) {
  protected def value: Array[A]

//def size: ST[S,Int] = ST(value.size)
  def size: ST[S,Int] = ST(value.length)

  def write(i: Int, a: A): ST[S,Unit] = new ST[S,Unit] {
    def run(s: S) = {
      value(i) = a
      ((), s)
    }
  }
  def read(i: Int): ST[S,A] = ST(value(i))

  def freeze: ST[S,List[A]] = ST(value.toList)

  /* Exercise 14.1 */
  def fill(xs: Map[Int,A]): ST[S,Unit] = {
    val init: ST[S,Unit] = ST(())
    xs.foldRight(init){
      case ((i, a), b) => b.flatMap{ _ => write(i, a) }
    }
  }
  /* Listing 14-8 */
  def swap(i: Int, j: Int): ST[S,Unit] = for {
    x <- read(i)
    y <- read(j)
    _ <- write(i, y)
    _ <- write(j, x)
  } yield ()
}

object STArray {
  def apply[S,A:Manifest](sz: Int, v: A): ST[S, STArray[S,A]] =
    ST(new STArray[S,A] {
      lazy val value = Array.fill(sz)(v)
    })

  /* Listing 14-7 */
  def fromList[S,A:Manifest](xs: List[A]): ST[S, STArray[S,A]] =
    ST(new STArray[S,A] {
      lazy val value = xs.toArray
    })
}

object Exercise_14_2 {
  def partition[S](arr: STArray[S,Int], n: Int, r: Int, pivot: Int): ST[S,Int] = {
    def init = ST[S, Unit](())

    def loop(pivotVal: Int, j: STRef[S, Int])(st: ST[S, Unit], i: Int): ST[S, Unit] =
      for {
        _ <- st
        iValue <- arr read i
        _ <-
          if (iValue >= pivotVal) init
          else for {
            jValue <- j.read
            _ <- arr.swap(i, jValue)
            _ <- j.write(jValue + 1)
          } yield ()
      } yield ()

    for {
      pivotVal <- arr.read(pivot)
      _ <- arr.swap(pivot, r)
      jRef <- STRef(n)
      _ <- (n until r).foldLeft(init){ loop(pivotVal, jRef) }
      j <- jRef.read
      _ <- arr.swap(j, r)
    } yield j
  }
  def qs[S](a: STArray[S,Int], n: Int, r: Int): ST[S,Unit] = {
    if (n < r) for {
      pi <- partition(a, n, r, n + (r - n) / 2)
      _ <- qs(a, n, pi - 1)
      _ <- qs(a, pi + 1, r) } yield ()
    else
      ST[S, Unit](())
  }

  def quicksort(xs: List[Int]): List[Int] =
    if (xs.isEmpty) xs else ST.runST(new RunnableST[List[Int]] {
      def apply[S] = for {
        arr <- STArray.fromList(xs)
        size <- arr.size
        _ <- qs(arr, 0, size - 1)
        sorted <- arr.freeze
      } yield sorted
    })
}
