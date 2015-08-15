package epsa.sandbox

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpResponse, HttpRequest}
import akka.stream.ActorMaterializer

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

object TestHttp extends App {

  val uri = "http://sggestion-ede.com/product/index.php?doc=fp&amf=2763651-2735662-2763562-6096103"

  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()

  val f: Future[HttpResponse] =
    Http().singleRequest(HttpRequest(uri = uri))

  val r = Await.result(f, 10.seconds)
  println(r)
  println(r.headers.mkString("\n"))
  println(Await.result(r.entity.toStrict(5.seconds), 5.seconds).data.utf8String)

}
