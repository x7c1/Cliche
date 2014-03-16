package so.protocol.x7c1.plum.lib

import org.specs2.mutable.Specification

object PlumLibraryTest extends Specification {

  PlumLibrary.getClass.getSimpleName should{
    "create message" in {
      val message = PlumLibrary createMessageFor "earthlings"
      "hello, earthlings!" === message
    }
  }
}

