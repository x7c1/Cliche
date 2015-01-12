package x7c1.parse.lib.query

class ParseColumn[A](label: String)(implicit provider: MethodProvider[A]){
  def in (values: A*): Expression = provider(label) in (values:_*)
  def is (value: A): Expression = provider(label) is value
  def > (value: A): LinableExpression[A] = provider(label) > value
  def >= (value: A): LinableExpression[A] = provider(label) >= value
  def < (value: A): LinableExpression[A] = provider(label) < value
  def <= (value: A): LinableExpression[A] = provider(label) <= value
}

trait MethodProvider[A]{
  def apply(label: String): ColumnMethod[A]
}

trait ColumnMethod[A]{
  def in (values: A*): Expression
  def is (value: A): Expression
  def > (value: A): LinableExpression[A]
  def >= (value: A): LinableExpression[A]
  def < (value: A): LinableExpression[A]
  def <= (value: A): LinableExpression[A]
}

trait ColumnDefinitions {
  def define[A](label: String)(implicit provider: MethodProvider[A]) = {
    new ParseColumn[A](label)
  }
}
