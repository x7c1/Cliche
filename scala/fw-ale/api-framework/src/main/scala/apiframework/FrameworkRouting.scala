package apiframework

import scala.reflect.runtime.universe._
import x7c1.salad.inspector.{TypeDigest, TypeReflector}

trait FrameworkRouting {

  def services: Seq[Any]

  def register[A: WeakTypeTag]: TypeDigest = TypeReflector.inspect[A]

  object | {
    def set [A: WeakTypeTag](service: A): A = service
  }

  def prepare[A](services: A*) = Seq(services:_*)

  def main(args: Array[String]): Unit = {
    println(services)
  }
}
