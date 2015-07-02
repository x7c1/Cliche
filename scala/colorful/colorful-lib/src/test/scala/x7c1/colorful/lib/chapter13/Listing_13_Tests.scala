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
