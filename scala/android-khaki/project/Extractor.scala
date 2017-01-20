object Extractor {
  def apply[A, B](f: A => Option[B]): Extractor[A, B] = new Extractor(f)

  type ==> [A, B] = Extractor[A, B]
}

class Extractor[A, B](f: A => Option[B]) {
  def unapply(x: A): Option[B] = f(x)
}
