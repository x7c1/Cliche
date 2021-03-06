package x7c1.colorful.lib.chapter14

import org.scalatest.{Matchers, FlatSpecLike}

class Exercise_14_1_Tests extends FlatSpecLike with Matchers {

  "fill" can "update local array" in {
    val runnable = new RunnableST[List[Int]] {
      def st[S]: ST[S, List[Int]] = for {
        s1 <- STArray(3, 333)
        _  <- s1.fill(Map(0 -> 111, 1 -> 222))
        s2 <- s1.freeze
      } yield s2

      override def apply[S]: ST[S, List[Int]] = st
    }
    ST.runST(runnable) shouldBe List(111, 222, 333)
  }
}

class Exercise_14_2_Tests extends FlatSpecLike with Matchers {

  import Exercise_14_2.quicksort
  "quicksort" can "sort list in order" in {
    List().filter(_ => false)
    quicksort(List(3, 1, 4, 5, 2)) shouldBe List(1, 2, 3, 4, 5)
  }
}
