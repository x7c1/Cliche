package org.example.cyan.ldr.status.survey

import org.apache.log4j.{FileAppender, Logger, PatternLayout}

object FeedStatusFileWriter {
  def apply(cacheFilePath: String): FeedStatusFileWriter = {
    val logger = {
      val appender = new FileAppender()
      appender setFile cacheFilePath
      appender setLayout new PatternLayout("%d %m%n")
      appender setAppend true
      appender activateOptions()

      val logger = Logger.getLogger(getClass.getSimpleName)
      logger addAppender appender
      logger
    }
    new FeedStatusFileWriter(logger)
  }
}

class FeedStatusFileWriter(logger: Logger) extends FeedStatusWriter {
  override def write(event: FeedStatusEvent) = {
    val message = "%3d %s %s".format(
      event.status,
      event.feed.xmlUrl,
      event.feed.title
    )
    logger info message
  }
}
