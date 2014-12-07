package x7c1.ale.lib

import org.scalatest.{FunSpec, Matchers}

class AleDomainLibraryTest extends FunSpec with Matchers {

  describe(AleDomainLibrary.getClass.getSimpleName){
    it("should create message"){
      val message = AleDomainLibrary createMessageFor "earthlings"
      message should be("hello, earthlings!")
    }
  }
}

