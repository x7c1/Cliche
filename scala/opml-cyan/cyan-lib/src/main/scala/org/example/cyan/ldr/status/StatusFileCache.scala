package org.example.cyan.ldr.status

import scala.io.Source

class StatusFileCache(
  cacheFilePath: String, cache: Map[String, Int]) extends StatusCache {

  override def get(url: String) = cache.get(url)

  override def countFeeds = cache.size

  override def countDeadFeeds = {
    cache.count { case (url, status) =>
      (status == -1) || (status >= 400)
    }
  }
  override def rebuild = StatusFileCache(cacheFilePath)
}

object StatusFileCache {
  def apply(cacheFilePath: String): StatusFileCache = {
    val source = Source fromFile cacheFilePath
    val pattern = """(-?\d+) (http[^ ]+)""".r
    val pairs = for(line <- source.getLines()) yield {
      pattern.
        findFirstMatchIn(line).
        filter(_.groupCount == 2).
        map{ matches =>
          val (status, url) = matches.group(1).toInt -> matches.group(2)
          url -> status
        }
    }
    val cache = Map(pairs.toSeq.flatten:_*)
    new StatusFileCache(cacheFilePath, cache)
  }
}
