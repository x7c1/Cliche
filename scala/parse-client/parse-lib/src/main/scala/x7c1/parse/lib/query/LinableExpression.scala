package x7c1.parse.lib.query

import spray.json.{JsObject, JsValue}

class LinableExpression[A](
  val column: String,
  value: JsValue,
  symbol: String) extends Expression {

  def and (expression: LinableExpression[A]): Expression = {
    val map = if (column == expression.column){
      Map(column -> JsObject(toSymbolMap ++ expression.toSymbolMap))
    } else {
      toMap ++ expression.toMap
    }
    Expression(map)
  }
  private def toSymbolMap = Map(symbol -> value)

  override def toMap = Map(column -> JsObject(toSymbolMap))
}
