package services

import javax.inject.Inject
import javax.inject.Singleton

import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

import play.api.mvc._
import play.api.libs.ws._
import play.api.http.HttpEntity

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl._
import akka.util.ByteString


@Singleton
class HelloService @Inject()(ws: WSClient) {
  def item(id: Int): String = {
    val request: WSRequest = ws.url(s"https://hacker-news.firebaseio.com/v0/item/$id.json")
    val response: Future[String] = request.get().map{ r => (r.json \ "title").as[String] }
    return Await.result(response, Duration.Inf)
  }
}
