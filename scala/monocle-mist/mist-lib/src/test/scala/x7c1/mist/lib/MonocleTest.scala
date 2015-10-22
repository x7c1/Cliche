package x7c1.mist.lib

import org.scalatest.{FunSpec, Matchers}

class MonocleTest extends FunSpec with Matchers {

  describe("monocle"){
    it("should create lenses"){
      import Company.address, Address.street, Street.name

      val x1 = Company(Address(Street(name = "sample1" )))
      val x2 = address composeLens street composeLens name set "sample2" apply x1

      x2.address.street.name shouldBe "sample2"
    }
  }
}
