package x7c1.brown.lib

import org.scalatest.{FunSpec, Matchers}

class BrownLibraryTest extends FunSpec with Matchers {

  describe(BrownLibrary.getClass.getSimpleName){
    it("should create message"){
      val message = BrownLibrary createMessageFor "earthlings"
      message should be("hello, earthlings!")
    }
  }
}
