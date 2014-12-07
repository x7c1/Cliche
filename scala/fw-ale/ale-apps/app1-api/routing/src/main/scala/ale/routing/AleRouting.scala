package ale.routing

import ale.app1.bar.v2.{BarRequestParameter, BarService}
import apiframework.FrameworkRouting

object AleRouting extends FrameworkRouting {

  override def services =
    prepare (
      | set BarService,
      | set BarService,
      | set BarService
    )

  def sample = register[BarRequestParameter]
}
