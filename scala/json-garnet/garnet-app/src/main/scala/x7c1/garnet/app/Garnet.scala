package x7c1.garnet.app

import x7c1.garnet.lib.json4s.GarnetJson4sLibrary

object Garnet extends App {
  val message = GarnetJson4sLibrary createMessageFor "Json4sGarnet"
  println(message)
}

