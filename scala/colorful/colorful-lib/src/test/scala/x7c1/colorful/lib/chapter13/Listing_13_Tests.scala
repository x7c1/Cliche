package x7c1.colorful.lib.chapter13

import org.scalatest.{FlatSpecLike, Matchers}

class Listing_13_22_Tests extends FlatSpecLike with Matchers {

  import Listing_13_15.Console.{printLn, readLn}
  import Listing_13_21._

  "runConsoleReader" can "emulate to input string" in {
    val free = for {
      _ <- printLn("I can only interact with the console.")
      ln1 <- readLn
      ln2 <- readLn
    } yield for {
      s1 <- ln1
      s2 <- ln2
    } yield {
      s1 + s2 + "!"
    }
    runConsoleReader(free).run("ha") shouldBe Some("haha!")
  }
}

class Listing_13_23_Tests extends FlatSpecLike with Matchers {

  import Listing_13_15.Console.{printLn, readLn}
  import Listing_13_23._

  "runConsoleState" can "emulate to read & print" in {
    val free = for {
      _ <- printLn("emulating console.")
      ln1 <- readLn
      ln2 <- readLn
    } yield for {
      s1 <- ln1
      s2 <- ln2
    } yield {
      s"$s1, $s2!"
    }
    val buffers = Buffers(
      in = List("hello", "world", "a", "b"),
      out = Vector("x")
    )
    val (s, b) = runConsoleState(free).run(buffers)
    s shouldBe Some("hello, world!")
    b shouldBe Buffers(
      in = List("a", "b"),
      out = Vector("x", "emulating console.")
    )
  }
}
