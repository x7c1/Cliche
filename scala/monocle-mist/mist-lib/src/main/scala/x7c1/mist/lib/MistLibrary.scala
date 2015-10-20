package x7c1.mist.lib

import scala.language.higherKinds
import monocle.macros.Lenses

@Lenses
case class Street(name: String)

@Lenses
case class Address(street: Street)

@Lenses
case class Company(address: Address)
