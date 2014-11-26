package x7c1.garnet.lib.spray

import spray.json.{JsValue, JsonParser}

object GarnetJsonSprayLibrary {

  def getJsonAst(source: String): JsValue = {
    JsonParser(source)
  }

  def getUser = {
    import spray.json.DefaultJsonProtocol._
    implicit val userFormat = jsonFormat2(SampleUser)

    val json = JsonParser("""{ "userId" : 123, "userName" : "John" }""")
    json.convertTo[SampleUser]
  }
  def getGenericUser = {
    /*
    // could not find implicit value for evidence parameter
    // of type spray.json.DefaultJsonProtocol.JF[x7c1.garnet.lib.spray.SampleValue[String]]
    import spray.json.DefaultJsonProtocol._
    implicit val genericUserFormat = jsonFormat3(SampleGenericUserImpl)

    val json = JsonParser("""{ "userId" : 345, "userValue" : 678, "sampleValue":{"x": "xyz"} }""")
    json.convertTo[SampleGenericUserImpl]
    */
  }
}

//object MyProtocol extends DefaultJsonProtocol {
//  implicit val userFormat: JsonFormat[SampleUser] = jsonFormat2(SampleUser)
//}

case class SampleUser(userId: Int, userName: String)

trait SampleGenericUser[A]{
  def userId: Int
  def userValue: A
  def sampleValue: SampleValue[A]
}

case class SampleGenericUserImpl(
  userId: Int,
  userValue: String,
  sampleValue: SampleValue[String]) extends SampleGenericUser[String]

case class SampleValue[A](x: A)