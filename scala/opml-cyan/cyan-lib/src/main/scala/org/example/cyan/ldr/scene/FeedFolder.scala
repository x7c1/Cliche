package org.example.cyan.ldr.scene

import org.example.cyan.ldr.Feed

import scala.xml.NodeSeq

trait FeedFolder {
  def feeds: Vector[Feed]
  def splitAt(n: Int): (FeedFolder, FeedFolder)
  def size: Int = feeds.size
}

object FeedFolder {
  def unapplySeq(folder: FeedFolder): Option[Vector[Feed]] = {
    Some(folder.feeds)
  }
}

object NamedFolder {
  def apply(name: String, feeds: NodeSeq): NamedFolder = {
    new NamedFolder(
      name = name,
      feeds = feeds.map{ Feed(_) }.toVector)
  }
}

class NamedFolder(
  val name: String,
  override val feeds: Vector[Feed]) extends FeedFolder {

  override def splitAt(n: Int): (NamedFolder, NamedFolder) = {
    val (left, right) = feeds.splitAt(n)
    new NamedFolder(name, left) -> new NamedFolder(name, right)
  }
  override def toString: String = s"[$name(${feeds.size})]"
}

class AnonFolder(val feeds: Vector[Feed] = Vector()) extends FeedFolder{
  def ++ (folder: AnonFolder): AnonFolder = {
    new AnonFolder(feeds ++ folder.feeds)
  }
  override def splitAt(n: Int): (AnonFolder, AnonFolder) = {
    val (left, right) = feeds.splitAt(n)
    new AnonFolder(left) -> new AnonFolder(right)
  }
  override def toString: String = s"+anon(${feeds.size})"
}
