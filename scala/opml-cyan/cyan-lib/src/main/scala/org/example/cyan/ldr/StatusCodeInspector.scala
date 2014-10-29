package org.example.cyan.ldr

import com.ning.http.client.AsyncHttpClientConfig
import play.api.libs.ws.ning.NingWSClient

import scala.concurrent.{ExecutionContext, Future}

object StatusCodeInspector {

  def createClient = {
    val timeout = 3000
    val config = new AsyncHttpClientConfig.Builder().
      setConnectionTimeoutInMs(timeout).
      setRequestTimeoutInMs(timeout).
      setIdleConnectionTimeoutInMs(timeout).
      setIdleConnectionInPoolTimeoutInMs(timeout).
      setWebSocketIdleTimeoutInMs(timeout).
      build()

    new NingWSClient(config)
  }
  def inspectStatus(url: String)(implicit context: ExecutionContext): Future[Int] = {
    val client = createClient
    val response = Future(url).flatMap{ url =>
      client.url(url).get()
    }
    response.onComplete{ case result => client.close() }
    response.map(_.status)
  }
}
