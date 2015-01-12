package x7c1.parse.lib.query

import spray.json.{JsObject, JsValue}

class ParseQuery private(expression: Expression*){
  def where(column: Expression*): ParseQuery = {
    new ParseQuery(expression ++ column :_*)
  }
  def jsonBody: String = {
    val default = Map[String, JsValue]()
    val map = expression.foldLeft(default){_ ++ _.toMap}
    JsObject("where" -> JsObject(map)).prettyPrint
  }
}

object ParseQuery {
  def apply(): ParseQuery = new ParseQuery()
}
