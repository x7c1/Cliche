package org.example.cyan.ldr.stream

import org.example.cyan.ldr.Feed

sealed trait OpmlEvent

case class FolderEvent(title: String) extends OpmlEvent

class FeedEvent(val feed: Feed) extends OpmlEvent {
  override def toString = s"${getClass.getSimpleName}(${feed.xmlUrl},..)"
}
