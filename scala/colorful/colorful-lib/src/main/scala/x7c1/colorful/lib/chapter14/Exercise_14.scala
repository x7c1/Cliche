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
