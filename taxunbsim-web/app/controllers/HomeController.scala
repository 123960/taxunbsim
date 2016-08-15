package controllers

import javax.inject._
import akka.actor._
import akka.stream._
import play.api._
import play.api.mvc._
import play.api.libs.streams._
import play.api.libs.json._

import actors._


/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
 @Singleton
 class HomeController @Inject() (implicit system: ActorSystem, implicit val materializer: Materializer) extends Controller {

   implicit val myCustomCharset = Codec.javaSupported("iso-8859-1")

  /**
   * Create an Action to render an HTML page with a welcome message.
   * The configuration in the `routes` file means that this method
   * will be called when the application receives a `GET` request with
   * a path of `/`.
   */
  def index = Action {
    Ok(views.html.main())
  }

  def wsGroup = WebSocket.accept[String, String] { request =>
    ActorFlow.actorRef(out => WebSocketActor.props(out))
  }

}
