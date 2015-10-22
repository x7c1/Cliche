package x7c1.mist.app

import x7c1.mist.lib._

object MonocleMist extends App {
  import Company.address, Address.street, Street.name

  val x1 = Company(Address(Street(name = "sample1" )))
  val x2 = address composeLens street composeLens name set "sample2" apply x1

  println(x2)// Company(Address(Street(sample2)))
}
