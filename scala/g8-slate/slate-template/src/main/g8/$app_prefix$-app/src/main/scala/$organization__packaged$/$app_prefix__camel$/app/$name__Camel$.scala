package $organization$

import $organization$.lib.$app_prefix;format="Camel"$Library

object $name;format="Camel"$ extends App {
  val message = $app_prefix;format="Camel"$Library createMessageFor "world"
  println(message)
}

