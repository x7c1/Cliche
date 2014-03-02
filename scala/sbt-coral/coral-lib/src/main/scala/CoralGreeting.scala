package coral.lib

object CoralGreeting {
  def message = "hello!!"

  def createMessage(name: String) = s"$message $name!!"
}
