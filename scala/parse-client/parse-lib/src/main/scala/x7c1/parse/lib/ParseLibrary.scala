package x7c1.parse.lib

import spray.json.{JsString, JsArray, JsNumber, JsValue, JsObject}

object ParseLibrary {
  def createMessageFor(name: String) = s"hello, $name!"
}

trait ColumnDefinitions {
  def define[A](label: String)(implicit methods: ColumnMethods[A]) = {
    new ParseColumn[A](label)
  }
}

trait DefaultAdapters
  extends StringColumnAdapter
  with IntColumnAdapter

trait StringColumnAdapter {
  implicit object ForStringColumn extends ColumnMethods[String] {

    override def in(label: String, values: String*) =
      new In(label: String, values.map(JsString.apply):_*)

    override def greaterThan(label: String, value: String) = ???

    override def greaterThanOrEqualTo(label: String, value: String) = ???

    override def lessThanOrEqualTo(label: String, value: String) = ???

    override def lessThan(label: String, value: String) = ???

    override def equalTo(label: String, value: String) =
      new Equal(label: String, JsString(value))
  }
}

trait IntColumnAdapter {
  implicit object ForIntColumn extends ColumnMethods[Int] {

    override def in(label: String, values: Int*) = ???

    override def greaterThan(label: String, value: Int) =
      new LinableExpression[Int](label, JsNumber(value), "$gt")

    override def greaterThanOrEqualTo(label: String, value: Int) =
      new LinableExpression[Int](label, JsNumber(value), "$gte")

    override def lessThanOrEqualTo(label: String, value: Int) =
      new LinableExpression[Int](label, JsNumber(value), "$lte")

    override def lessThan(label: String, value: Int) =
      new LinableExpression[Int](label, JsNumber(value), "$lt")

    override def equalTo(label: String, value: Int) = ???
  }
}

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

class Equal(label: String, value: JsValue) extends Expression {
  override def toMap = Map(label -> value)
}

class LinableExpression[A](
  val column: String,
  value: JsValue,
  symbol: String) extends Expression {

  def and (cond: LinableExpression[A]): Expression = {
    val map =
      if (column == cond.column){
        val map = toSymbolMap ++ cond.toSymbolMap
        Map(column -> JsObject(map))
      } else {
        toMap ++ cond.toMap
      }

    Expression(map)
  }
  private def toSymbolMap = Map(symbol -> value)

  override def toMap = Map(column -> JsObject(toSymbolMap))
}

trait ColumnMethods[A]{
  def equalTo (label: String, value: A): Expression

  def in (label: String, values: A*): Expression

  def greaterThan (label: String, value: A): LinableExpression[A]

  def greaterThanOrEqualTo (label: String, value: A): LinableExpression[A]

  def lessThan (label: String, value: A): LinableExpression[A]

  def lessThanOrEqualTo (label: String, value: A): LinableExpression[A]
}

class ParseColumn[A](label: String)(implicit methods: ColumnMethods[A]){
  def in (values: A*): Expression = methods.in(label, values:_*)
  def > (value: A): LinableExpression[A] = methods.greaterThan(label, value)
  def >= (value: A): LinableExpression[A] = methods.greaterThanOrEqualTo(label, value)
  def < (value: A): LinableExpression[A] = methods.lessThan(label, value)
  def <= (value: A): LinableExpression[A] = methods.lessThanOrEqualTo(label, value)
  def is (value: A): Expression = methods.equalTo(label, value)
}
