package x7c1.parse.lib.query

import spray.json.{JsArray, JsObject, JsValue}

trait Expression {
  def toMap: Map[String, JsValue]
}

object Expression {
  def apply(map: Map[String, JsValue]): Expression = {
    new ExpressionImpl(map)
  }
  private class ExpressionImpl(val toMap: Map[String, JsValue]) extends Expression
}

class In[A](label: String, values: JsValue*) extends Expression {
  override def toMap = {
    Map(label -> JsObject("$in" -> JsArray(values:_*)))
  }
}

class Is(label: String, value: JsValue) extends Expression {
  override def toMap = Map(label -> value)
}
