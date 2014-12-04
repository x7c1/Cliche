package x7c1.garnet.lib.spray

import org.scalatest.{FunSpec, Matchers}
import spray.json.{JsValue, JsNumber}

class GarnetSprayJsonLibraryTest extends FunSpec with Matchers {

  describe(GarnetSprayJsonLibrary.getClass.getSimpleName){
    it("should parse json"){
      val json: JsValue = GarnetSprayJsonLibrary.getJsonAst(""" { "x1" : 321 } """)
      val (key, JsNumber(value)) = json.asJsObject.fields.head
      key shouldBe "x1"
      value shouldBe 321
    }
    it("should convert json to instance of case class"){
      val x = GarnetSprayJsonLibrary.getUser
      x.userId shouldBe 123
      x.userName shouldBe "John"
    }
    it("should generate json from trait-instance"){
      val struct = GarnetSprayJsonLibrary.getSampleStructure
      struct.x === 123
      struct.y === "hoge-"
    }
  }
}
