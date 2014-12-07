package x7c1.taupe.lib

import org.scalatest.{FunSpec, Matchers}

class TaupeLibraryTest extends FunSpec with Matchers {

  describe(TaupeLibrary.getClass.getSimpleName){
    it("should create message"){
      val message = TaupeLibrary createMessageFor "earthlings"
      message should be("hello, earthlings!")
    }
  }
}

