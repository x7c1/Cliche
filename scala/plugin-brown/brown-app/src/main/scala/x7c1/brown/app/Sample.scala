package x7c1.brown.app

class Sample[A](val sampleValue: A) extends Hoge with Fuga{
  def sample = "sample"
}

trait Hoge {
  def hoge = 1
}

trait Fuga {
  def fuga = "fuga"
}

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
