package x7c1.brown.app

trait UserResponse extends Response[Seq[User]]{
}

trait Response[A]{
  def content: A
  def page: Page
}

trait User {
  def userId: Int
  def name: String
}

trait Page {
  def currentPage: Int
  def totalPages: Int
}

trait Foo[A, B] {
  def foo: ((A, B), (A, B))
}

trait Bar {
  def bar = "bar"
}

trait Baz[A] {
  def baz: A => A
}

trait MergedSample extends Foo[String, Int] with Bar with Baz[Long]
