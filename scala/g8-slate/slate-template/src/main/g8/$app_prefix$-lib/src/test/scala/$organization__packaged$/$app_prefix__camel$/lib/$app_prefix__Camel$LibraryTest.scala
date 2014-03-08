package $organization$.$app_prefix$.lib

import org.specs2.mutable.Specification

object $app_prefix;format="Camel"$LibraryTest extends Specification {

  $app_prefix;format="Camel"$Library.getClass.getSimpleName should{
    "create message" in {
      val message = $app_prefix;format="Camel"$Library createMessageFor "earthlings"
      "hello, earthlings!" === message
    }
  }
}

