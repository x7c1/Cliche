package org.example.cyan.ldr.status.survey

object FeedStatusConsoleWriter extends FeedStatusWriter{
  override def write(event: FeedStatusEvent) = {
    val message = "%3d %s %s".format(
      event.status,
      event.feed.host.getOrElse(""),
      event.feed.title
    )
    Console println message
  }
}
