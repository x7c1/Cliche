package x7c1.brown.app

import x7c1.brown.lib.BrownLibrary

object PluginBrown extends App {
  val message = BrownLibrary createMessageFor "PluginBrown"
  val hoge = new Sample(message)
  println(hoge.sampleValue)
}
