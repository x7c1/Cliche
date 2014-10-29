package org.example.cyan.ldr.status.survey

import org.example.cyan.ldr.status.{StatusCache, StatusFileCache}
import org.example.cyan.ldr.{Feed, StatusCodeInspector}

import scala.annotation.tailrec
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

object FeedStatusSurvey {
  def fromFile(cacheFilePath: String): FeedStatusSurvey = {
    new FeedStatusSurvey(
      writers = Seq(FeedStatusFileWriter(cacheFilePath)),
      cache = StatusFileCache(cacheFilePath) )
  }
  def withConsole(cacheFilePath: String): FeedStatusSurvey = {
    fromFile(cacheFilePath) on FeedStatusConsoleWriter
  }
}

class FeedStatusSurvey(writers: Seq[FeedStatusWriter], cache: StatusCache){

  def updateBy(feeds: Vector[Feed])(implicit context: ExecutionContext): FeedStatusSurvey = {
    def request(feed: Feed): Future[Unit] = {
      val eventFuture = {
        val toEvent = FeedStatusEvent(_: Int, feed)
        val status = StatusCodeInspector inspectStatus feed.xmlUrl
        status.map(toEvent) fallbackTo Future(toEvent(-1))
      }
      eventFuture onSuccess {
        case event => writers foreach (_ write event)
      }
      eventFuture.transform(_ => {}, identity)
    }
    @tailrec
    def inspect(feeds: Vector[Feed], done: Future[Unit] = Future{}): Future[Unit] = {
      feeds match {
        case Vector(feed) =>
          done.flatMap(_ => request(feed))
        case feed +: tail =>
          val response = request(feed)
          Await.result(response, Duration.Inf)
          Thread sleep 500
          inspect(tail, done.flatMap(_ => response))
        case Vector() =>
          done
      }
    }
    val isTarget = (feed: Feed) => feed.host.isDefined && !has(feed)
    val requests = feeds filter isTarget groupBy (_.host.get) map {
      case (host, hostFeeds) => Future(hostFeeds).flatMap(inspect(_))
    }
    while(requests.exists(!_.isCompleted)){
      Thread sleep 1000
    }
    new FeedStatusSurvey(writers, cache.rebuild)
  }
  def on(writer: FeedStatusWriter) = {
    new FeedStatusSurvey(writers :+ writer , cache)
  }
  def has(feed: Feed): Boolean = cache.get(feed.xmlUrl).isDefined

  def countFeeds: Int = cache.countFeeds

  def countDeadFeeds: Int = cache.countDeadFeeds
}
