package org.example.cyan.ldr.status

trait StatusCache {
   def countFeeds: Int
   def countDeadFeeds: Int
   def get(url: String): Option[Int]
   def rebuild: StatusCache
 }
