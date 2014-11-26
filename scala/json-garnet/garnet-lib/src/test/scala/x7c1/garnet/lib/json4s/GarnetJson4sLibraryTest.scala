package x7c1.garnet.lib.json4s

import org.json4s.JsonAST.{JInt, JObject}
import org.scalatest.{FunSpec, Matchers}

class GarnetJson4sLibraryTest extends FunSpec with Matchers {

  describe(GarnetJson4sLibrary.getClass.getSimpleName){
    it("should create message"){
      val message = GarnetJson4sLibrary createMessageFor "earthlings"
      message should be("hello, earthlings!")
    }
    it("should parse json"){
      val json = GarnetJson4sLibrary.getJsonAst("""{ "x1": 321 }""")
      val JObject(List((key, JInt(value)))) = json
      key shouldBe "x1"
      value shouldBe 321
    }
    it("should convert json to instance of case class"){
      val x = GarnetJson4sLibrary.getUser
      x.userId shouldBe 123
      x.userName shouldBe "John"
    }
    it("should convert json to instance of (case class <: generic trait)"){
      val x = GarnetJson4sLibrary.getGenericUser
      x.userId shouldBe 345
      x.userValue shouldBe "678"
      x.sampleValue.x shouldBe "xyz"
    }
  }
}
