package x7c1.parse.lib

import spray.json.{JsObject, JsValue}

class ParseQuery private(expr: Expression*){
  def where(column: Expression*): ParseQuery = {
    new ParseQuery(expr ++ column :_*)
  }
  def jsonBody: String = {
    val default = Map[String, JsValue]()
    val map = expr.foldLeft(default){_ ++ _.toMap}
    JsObject("where" -> JsObject(map)).prettyPrint
  }
}

object ParseQuery {
  def apply(): ParseQuery = new ParseQuery()
}
