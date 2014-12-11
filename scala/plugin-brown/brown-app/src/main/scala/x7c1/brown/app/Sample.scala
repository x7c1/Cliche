package x7c1.brown.app

import java.util.Date

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
  def currentTime: Date
}

trait Baz[A] {
  def baz: A => A

  trait InnerTypeBaz {
    def innerBazValue: Option[A]
  }
}

trait MergedSample extends Foo[String, Int] with Bar with Baz[Long]
