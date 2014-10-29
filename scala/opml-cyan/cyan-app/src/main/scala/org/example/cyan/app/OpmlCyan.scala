package org.example.cyan.app

import org.example.cyan.ldr.scene.FeedScene
import org.example.cyan.ldr.status.survey.FeedStatusSurvey

import scala.util.Random

object OpmlCyan extends App {

  def test1() = {
    val scene = FeedScene fromFile args(0)
    scene.folders foreach { f =>
      println(s"${f.name}, ${f.feeds.size}")
    }
    scene splitEvery 1000 foreach println
    println(
      scene.allFeeds.count{_.xmlUrl contains "tumblr.com/rss"}
    )
  }

  def test3(): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global

    val scene = FeedScene fromFile args(0)
    val allFeeds = scene.allFeeds
    val targets = Random shuffle allFeeds take 20

    val existingSurvey = FeedStatusSurvey withConsole "feed-status.log"
    val unknownFeeds = targets filterNot existingSurvey.has
    val latestSurvey = existingSurvey updateBy unknownFeeds

    println(
      s"unknown(${unknownFeeds.length}/${targets.length}), " +
      s"original(${existingSurvey.countFeeds})"
    )
    println(
      s"unreachable(${latestSurvey.countDeadFeeds}), " +
      s"inspected(${latestSurvey.countFeeds}/${allFeeds.length})"
    )
  }
  test3()

  println("done!")
}
