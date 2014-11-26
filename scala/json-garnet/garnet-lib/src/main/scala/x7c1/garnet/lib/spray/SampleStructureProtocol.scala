package x7c1.garnet.lib.spray

import spray.json.{DefaultJsonProtocol, JsNumber, JsObject, JsString, JsValue, RootJsonFormat}

object SampleStructureProtocol extends DefaultJsonProtocol {
  implicit object SampleStructureFormat extends RootJsonFormat[SampleStructure]{
    override def write(obj: SampleStructure) =
      JsObject(
        "x" -> JsNumber(obj.x),
        "y" -> JsString(obj.y)
      )
    override def read(json: JsValue) = json match {
      case JsObject(map) =>
        val Some(JsNumber(x)) = map.get("x")
        val Some(JsString(y)) = map.get("y")
        SampleStructure(
          x.toFloat,
          y
        )
      case _ => spray.json.deserializationError("SampleStructure expected")
    }
  }
}
