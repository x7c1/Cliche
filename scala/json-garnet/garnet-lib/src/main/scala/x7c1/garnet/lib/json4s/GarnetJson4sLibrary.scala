package x7c1.garnet.lib.json4s

import org.json4s.DefaultFormats
import org.json4s.native.JsonMethods.parse

object GarnetJson4sLibrary {
  def createMessageFor(name: String) = s"hello, $name!"

  def getJsonAst(body: String) = {
    parse(body)
  }

  def getUser = {
    implicit val formats = DefaultFormats
    val json = parse("""{ "userId" : 123, "userName" : "John" }""")
    json.extract[SampleUser]
  }
  def getGenericUser = {
    implicit val formats = DefaultFormats
    val json = parse("""{ "userId" : 345, "userValue" : 678, "sampleValue":{"x": "xyz"} }""")
    json.extract[SampleGenericUserImpl]
  }
}

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