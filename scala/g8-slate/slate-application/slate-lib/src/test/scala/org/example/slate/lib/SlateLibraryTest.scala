package org.example.slate.lib

import org.specs2.mutable.Specification

object SlateLibraryTest extends Specification {

  SlateLibrary.getClass.getSimpleName should{
    "create message" in {
      val message = SlateLibrary createMessageFor "earthlings"
      "hello, earthlings!" === message
    }
  }
}

