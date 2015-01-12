package x7c1.parse.app

import x7c1.parse.lib.ParseLibrary

object ParseLibClient extends App {
  val message = ParseLibrary createMessageFor "ParseClient"
  println(message)
}
