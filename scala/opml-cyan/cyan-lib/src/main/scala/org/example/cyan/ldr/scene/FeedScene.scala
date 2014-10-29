package org.example.cyan.ldr.scene

import java.io.File

import org.example.cyan.ldr.Feed

import scala.annotation.tailrec
import scala.xml.Document
import scala.xml.parsing.ConstructingParser

class FeedScene (
  val folders: Vector[NamedFolder],
  val anonFolder: AnonFolder ){

  def allFeeds: Vector[Feed] = {
    folders.map(_.feeds).flatten ++ anonFolder.feeds
  }
  def folderOf(name: String): Option[NamedFolder] = {
    folders.find(_.name == name)
  }
  def splitEvery(max: Int): Vector[FeedScene] = {
    @tailrec
    def slice(
      folders: Vector[FeedFolder], scenes: Vector[FeedScene],
      others: Vector[FeedFolder] = Vector()): Vector[FeedScene] = {

      import org.example.cyan.ldr.scene.FeedScene.{fromFolders => Scene}
      folders match {
        case folder +: tail =>
          folder.splitAt(max - others.map(_.size).sum) match {
            case (left, FeedFolder()) =>
              slice(tail, scenes, others :+ left)
            case (left, right) =>
              slice(right +: tail, scenes :+ Scene(others :+ left))
          }
        case Vector() => scenes :+ Scene(others)
      }
    }
    slice(folders :+ anonFolder, Vector())
  }
  override def toString: String = {
    val summary = (folders :+ anonFolder).map{"" + _}.mkString(",")
    summary
  }
}

object FeedScene {
  def fromFolders(folders: Vector[FeedFolder]): FeedScene = {
    val anon = folders.collect{ case f: AnonFolder => f }
    new FeedScene(
      folders = folders.collect{ case f: NamedFolder => f },
      anonFolder = anon.foldLeft(new AnonFolder){_ ++ _}
    )
  }
  def fromFile(path: String): FeedScene = {
    val parser = ConstructingParser.fromFile(new File(path), preserveWS = false)
    FeedScene fromDocument parser.document()
  }
  def fromDocument(document: Document): FeedScene = {
    val outlines = document \ "body" \ "outline" \ "outline"
    val (folders, feeds) = outlines.partition{ _.attribute("xmlUrl").isEmpty }
    val namedFolders = folders.map{ folder =>
      NamedFolder(
        name = folder \@ "title",
        feeds = folder \ "outline" )
    }
    new FeedScene(
      folders = namedFolders.toVector,
      anonFolder = new AnonFolder(feeds.map{Feed(_)}.toVector)
    )
  }
}
