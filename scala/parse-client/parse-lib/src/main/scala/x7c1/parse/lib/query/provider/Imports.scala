package x7c1.parse.lib.query.provider

import spray.json.{JsNumber, JsString}
import x7c1.parse.lib.query.{Is, In, LinableExpression, ColumnMethod, MethodProvider}

object Imports {
  implicit val forIntColumn = ForIntColumn
  implicit val forStringColumn = ForStringColumn
}

object ForStringColumn extends MethodProvider[String] {
  override def apply(label: String): ColumnMethod[String] = new Method(label)

  private class Method(label: String) extends ColumnMethod[String]{

    override def in(values: String*) =
      new In(label: String, values.map(JsString.apply):_*)

    override def is(value: String) =
      new Is(label: String, JsString(value))

    override def >(value: String) = ???

    override def <(value: String) = ???

    override def >=(value: String) = ???

    override def <=(value: String) = ???
  }
}

object ForIntColumn extends MethodProvider[Int] {
  override def apply(label: String): ColumnMethod[Int] = new Method(label)

  private class Method(label: String) extends ColumnMethod[Int] {

    override def in(values: Int*) = ???

    override def is(value: Int) = ???

    override def >(value: Int) =
      new LinableExpression[Int](label, JsNumber(value), "$gt")

    override def >=(value: Int) =
      new LinableExpression[Int](label, JsNumber(value), "$gte")

    override def <=(value: Int) =
      new LinableExpression[Int](label, JsNumber(value), "$lte")

    override def <(value: Int) =
      new LinableExpression[Int](label, JsNumber(value), "$lt")
  }
}
