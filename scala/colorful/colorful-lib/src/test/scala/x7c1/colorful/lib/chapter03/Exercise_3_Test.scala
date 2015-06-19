package x7c1.colorful.lib.chapter03

import fpinscala.datastructures._
import org.scalatest.{FunSpec, Matchers}

class Exercise_3_1_Test  extends FunSpec with Matchers {
  it("can return value"){
    Exercise_3_1.call() shouldBe 3
  }
}

class Exercise_3_2_Test  extends FunSpec with Matchers {
  import Exercise_3_2.tail

  it("can call tail"){
    tail(List(1, 2, 3)) shouldBe List(2, 3)
  }
  it ("throws an exception through tail(Nil)"){
    intercept[UnsupportedOperationException]{ tail(Nil) }
  }
}

class Exercise_3_3_Test  extends FunSpec with Matchers {
  import Exercise_3_3.setHead

  it("can call setHead"){
    setHead(List(1, 2, 3), 0) shouldBe List(0, 2, 3)
  }
  it ("throws an exception through Nil"){
    intercept[UnsupportedOperationException]{ setHead(Nil, 0) }
  }
}

class Exercise_3_4_Test  extends FunSpec with Matchers {
  import Exercise_3_4.drop

  it("can call drop"){
    drop(List(1, 2, 3),  1) shouldBe List(2, 3)
    drop(List(1, 2, 3),  0) shouldBe List(1, 2, 3)
    drop(List(1, 2, 3), -1) shouldBe List(1, 2, 3)
    drop(List(1, 2, 3),  4) shouldBe Nil

    drop(Nil, 4) shouldBe Nil
  }
}

class Exercise_3_5_Test  extends FunSpec with Matchers {
  import Exercise_3_5.dropWhile

  val f = (x: Int => Boolean) => x

  it("can call dropWhile") {
    dropWhile(List(1, 2, 3), f(_ < 3)) shouldBe List(3)
    dropWhile(List(1, 2, 3), f(_ > 3)) shouldBe List(1, 2, 3)
    dropWhile(Nil, f(_ < 3)) shouldBe Nil
  }
}

class Exercise_3_6_Test  extends FunSpec with Matchers {
  import Exercise_3_6.init

  it("can call init"){
    init(List(1, 2, 3, 4)) shouldBe List(1, 2, 3)
  }
  it("throws an exception through Nil"){
    intercept[UnsupportedOperationException]{ init(Nil) }
  }
}

class Exercise_3_8_Test  extends FunSpec with Matchers {
  it("can return value"){
    Exercise_3_8.call() shouldBe List(1, 2, 3)
  }
}

class Exercise_3_9_Test  extends FunSpec with Matchers {
  import Exercise_3_9.{length => length_}

  it("can call length"){
    length_(List(1, 2, 3)) shouldBe 3
    length_(List()) shouldBe 0
  }
}

class Exercise_3_10_Test  extends FunSpec with Matchers {
  import Exercise_3_10.foldLeft

  it("can call foldLeft"){
    foldLeft(List("a", "b", "c"), "!"){_ + _} shouldBe "!abc"
    foldLeft(List[String](), "a"){_ + _} shouldBe "a"
  }
}

class Exercise_3_11_Test  extends FunSpec with Matchers {
  import Exercise_3_11.{length => length_, product, sum}

  it("can call sum"){
    sum(List(1, 2, 3)) shouldBe 6
    sum(List()) shouldBe 0
  }
  it("can call product"){
    product(List(1, 2, 3, 4)) shouldBe 24
    product(List()) shouldBe 1.0
  }
  it("can call length"){
    length_(List(1, 2, 3)) shouldBe 3
    length_(List()) shouldBe 0
  }
}

class Exercise_3_12_Test  extends FunSpec with Matchers {
  import Exercise_3_12.reverse

  it("can call reverse"){
    reverse(List(1, 2, 3)) shouldBe List(3, 2, 1)
    reverse(List()) shouldBe List()
  }
}

class Exercise_3_13_Test  extends FunSpec with Matchers {
  import Exercise_3_13.{foldLeft2, foldLeft3, foldRight2, foldRight3}

  it("can call foldLeft2"){
    foldLeft2(List("a", "b", "c"), "!"){_ + _} shouldBe "!abc"
    foldLeft2(List[String](), "a"){_ + _} shouldBe "a"
  }
  it("can call foldLeft3"){
    foldLeft3(List("a", "b", "c"), "!"){_ + _} shouldBe "!abc"
    foldLeft3(List[String](), "a"){_ + _} shouldBe "a"
  }
  it("can call foldRight2"){
    foldRight2(List("a", "b", "c"), "!"){_ + _} shouldBe "abc!"
    foldRight2(List[String](), "a"){_ + _} shouldBe "a"
  }
  it("can call foldRight3"){
    foldRight3(List("a", "b", "c"), "!"){_ + _} shouldBe "abc!"
    foldRight3(List[String](), "a"){_ + _} shouldBe "a"
  }
}

class Exercise_3_14_Test  extends FunSpec with Matchers {
  import Exercise_3_14.{append2, append3}

  it("can call append2"){
    append2(List(1, 2), List(3, 4)) shouldBe List(1, 2, 3, 4)
  }
  it("can call append3"){
    append3(List(1, 2), List(3, 4)) shouldBe List(1, 2, 3, 4)
  }
}

class Exercise_3_15_Test  extends FunSpec with Matchers {
  import Exercise_3_15.appendAll

  it("can call appendAll"){
    val list = List(
      List(0, 1), List(2, 3), List(4, 5)
    )
    appendAll(list) shouldBe List(0, 1, 2, 3, 4, 5)
  }
}

class Exercise_3_16_Test  extends FunSpec with Matchers {
  import Exercise_3_16.addOne

  it("can call add"){
    addOne(List(1, 2, 3)) shouldBe List(2, 3, 4)
  }
}

class Exercise_3_17_Test  extends FunSpec with Matchers {
  import Exercise_3_17.convertToString

  it("can call convertToString"){
    convertToString(List(1.0, 2.0)) shouldBe List("1.0", "2.0")
  }
}

class Exercise_3_18_Test  extends FunSpec with Matchers {
  import Exercise_3_18.map

  it("can call map"){
    map(List(1, 2)){_ + 1} shouldBe List(2, 3)
  }
}

class Exercise_3_19_Test  extends FunSpec with Matchers {
  import Exercise_3_19.filter

  it("can call filter"){
    filter(List(1, 2, 3)){_ % 2 == 0} shouldBe List(2)
  }
}

class Exercise_3_20_Test  extends FunSpec with Matchers {
  import Exercise_3_20.flatMap

  it("can call flatMap"){
    flatMap(List(1,2,3))(i => List(i, i)) shouldBe List(1,1,2,2,3,3)
  }
}

class Exercise_3_21_Test  extends FunSpec with Matchers {
  import Exercise_3_21.filter2

  it("can call filter2"){
    filter2(List(1, 2, 3)){_ % 2 == 0} shouldBe List(2)
  }
}

class Exercise_3_22_Test  extends FunSpec with Matchers {
  import Exercise_3_22.add

  it("can call add"){
    add(List(1, 2, 3), List(4, 5, 6)) shouldBe List(5, 7, 9)
  }
}

class Exercise_3_23_Test  extends FunSpec with Matchers {
  import Exercise_3_23.zipWith

  it("can call zipWith"){
    zipWith(List(1, 2, 3), List(4, 5, 6))(_ + _) shouldBe List(5, 7, 9)
  }
}

class Exercise_3_24_Test  extends FunSpec with Matchers {
  import Exercise_3_24.hasSubsequence

  it("can call hasSubsequence"){
    hasSubsequence(List(1, 2, 3, 4), List(1, 2)) shouldBe true
    hasSubsequence(List(1, 2, 3, 4), List(2, 3)) shouldBe true
    hasSubsequence(List(1, 2, 3, 4), List(4)) shouldBe true

    hasSubsequence(List(1, 2, 3, 4), List(5)) shouldBe false
    hasSubsequence(List(), List(5)) shouldBe false
  }
}

class Exercise_3_25_Test  extends FunSpec with Matchers {
  import Exercise_3_25.{size => size_}

  it("can call size"){
    val tree = Branch(
      Branch(Leaf("a"), Leaf("b")),
      Branch(Leaf("c"), Leaf("d"))
    )
    size_(tree) shouldBe 7
  }
}

class Exercise_3_26_Test  extends FunSpec with Matchers {
  import Exercise_3_26.maximum

  it("can call maximum"){
    val tree = Branch(
      Branch(Leaf(1), Leaf(4)),
      Branch(Leaf(2), Leaf(5))
    )
    maximum(tree) shouldBe 5
  }
}

class Exercise_3_27_Test  extends FunSpec with Matchers {
  import Exercise_3_27.depth

  it("can call depth"){
    val tree = Branch(
      Branch(Leaf(1), Leaf(4)),
      Branch(Leaf(2), Leaf(5))
    )
    depth(tree) shouldBe 2
  }
}

class Exercise_3_28_Test  extends FunSpec with Matchers {
  import Exercise_3_28.map

  it("can call map"){
    val tree = Branch(
      Branch(Leaf(1), Leaf(4)),
      Branch(Leaf(2), Leaf(5))
    )
    map(tree){_ + 1} shouldBe Branch(
      Branch(Leaf(2), Leaf(5)),
      Branch(Leaf(3), Leaf(6))
    )
  }
}

class Exercise_3_29_Test  extends FunSpec with Matchers {
  import Exercise_3_29.{size => size_, _}

  val tree = Branch(
    Branch(Leaf(1), Leaf(4)),
    Branch(Leaf(2), Leaf(5))
  )
  it("can call size"){
    size_(tree) shouldBe 7
  }
  it("can call maximum"){
    maximum(tree) shouldBe 5
  }
  it("can call depth"){
    depth(tree) shouldBe 2
  }
  it("can call map"){
    map(tree){_ + 1} shouldBe Branch(
      Branch(Leaf(2), Leaf(5)),
      Branch(Leaf(3), Leaf(6))
    )
  }
}
