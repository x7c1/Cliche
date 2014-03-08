package coral.lib

import org.specs2.mutable.Specification

object CoralGreetingTest extends Specification {

  CoralGreeting.getClass.getSimpleName should {
    "create message" in {
      val message = CoralGreeting createMessage "world"
      "hello!! world!!" === message
    }
  }
}
