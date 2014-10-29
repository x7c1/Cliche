package org.example.cyan.ldr.stream

import org.example.cyan.ldr.Feed

import scala.io.Source
import scala.xml.MetaData
import scala.xml.pull.{EvElemStart, XMLEventReader}


object OpmlStream {

  def from(filePath: String): Stream[OpmlEvent] = {
    val reader = new XMLEventReader(Source fromFile filePath)
    val outlines = for (event <- reader) yield event match {
      case EvElemStart(pre, "outline", attrs, scope) =>
        toParsers(attrs).find(_.isTarget).map(_.getEvent)
      case _ =>
        None
    }
    // ignore <outline title="Subscriptions"> at the head
    outlines.toStream.flatten.tail
  }

  private def toParsers(attrs: MetaData) = Seq(
    new FeedOutlineParser(attrs),
    new FolderOutlineParser(attrs)
  )

  private trait OutlineParser {
    def isTarget: Boolean
    def getEvent: OpmlEvent
  }

  private class FolderOutlineParser(attrs: MetaData) extends OutlineParser {
    private val title = attrs.get("title")
    override def isTarget = title.isDefined && attrs.get("xmlUrl").isEmpty
    override def getEvent = FolderEvent(title.get.head.text)
  }

  private class FeedOutlineParser(attrs: MetaData) extends OutlineParser {
    private val xmlUrl = attrs.get("xmlUrl")

    override def isTarget = xmlUrl.isDefined

    override def getEvent = new FeedEvent(new Feed(
      title = attrs.get("title").map(_.head.text).getOrElse(""),
      xmlUrl = xmlUrl.get.head.text,
      htmlUrl = attrs.get("htmlUrl").map(_.head.text).getOrElse("")
    ))
  }

}
