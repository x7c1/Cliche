package org.example.cyan.ldr.status.survey

trait FeedStatusWriter {
  def write(event: FeedStatusEvent): Unit
}
