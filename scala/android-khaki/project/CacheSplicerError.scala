import sbt.File

sealed trait CacheSplicerError {
  def message: String
}

object CacheSplicerError {

  case class NotFound(file: File) extends CacheSplicerError {
    override def message: String = s"file not found: $file"
  }

  case class Messaged(message: String) extends CacheSplicerError

  case class Unexpected(cause: Exception) extends CacheSplicerError {
    override def message: String = cause.getMessage
  }

}