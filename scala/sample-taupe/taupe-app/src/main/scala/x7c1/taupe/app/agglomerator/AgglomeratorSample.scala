package x7c1.taupe.app.agglomerator

import x7c1.salad.parameter.Agglomerator.define
import scala.language.reflectiveCalls

object AgglomeratorSample extends App {

  def show = define { (id: Int, name: String) =>
    s"id:$id, name:$name"
  }
  println(show(123, "foo"))// id:123, name:foo

  case class User(id: Int, name: String, nick: String)
  val user = User(
    id = 123,
    name = "foo",
    nick = "bar"
  )
  println(show(user))// id:123, name:foo
}
