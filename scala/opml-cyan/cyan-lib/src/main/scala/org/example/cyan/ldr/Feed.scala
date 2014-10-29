package org.example.cyan.ldr

import scala.xml.Node

class Feed(
  val title: String,
  val xmlUrl: String,
  val htmlUrl: String )
{
  lazy val host: Option[String] = {
    """^http[s]?://([^/]+)/.*""".r.
      findFirstMatchIn(xmlUrl).
      filter{ _.groupCount == 1 }.
      map{ _.group(1) }
  }
  override def toString: String = s"$title,$xmlUrl,$htmlUrl"
}

object Feed {
  def apply(node: Node): Feed = {
    new Feed(
      title = node \@ "title",
      xmlUrl = node \@ "xmlUrl",
      htmlUrl = node \@ "htmlUrl" )
  }
}
