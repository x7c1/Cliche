package x7c1.parse.lib

class ParseNotifierClient(credentials: Credentials){
}

object ParseNotifierClient {
  def using(credentials: Credentials): ParseNotifierClient = {
    new ParseNotifierClient(credentials)
  }
}

class Credentials(
  val applicationId: String,
  val apiKey: String )
