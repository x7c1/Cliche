package org.example.slate.app

import org.example.slate.lib.SlateLibrary

object SlateApplication extends App {
  val message = SlateLibrary createMessageFor "world"
  println(message)
}

