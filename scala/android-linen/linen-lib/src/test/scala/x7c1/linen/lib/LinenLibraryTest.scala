package x7c1.linen.lib

import org.scalatest.{FunSpec, Matchers}

class LinenLibraryTest extends FunSpec with Matchers {

  describe(LinenLibrary.getClass.getSimpleName){
    it("should create message"){
      val message = LinenLibrary createMessageFor "earthlings"
      message should be("hello, earthlings!")
    }
  }
}

