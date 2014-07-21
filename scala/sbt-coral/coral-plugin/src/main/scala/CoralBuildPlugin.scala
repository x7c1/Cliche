package coral.plugin

import coral.lib.CoralGreeting
import sbt.TaskKey

object CoralBuildPlugin extends sbt.Plugin{

  def message = CoralGreeting.createMessage("Mr.Plugin")

  object CoralTask {
    def hello = TaskKey[Unit]("coralHello", "sample greeting")
    def settings = Seq(
      hello := println(message)
    )
  }
}
