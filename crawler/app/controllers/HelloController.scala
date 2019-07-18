package controllers

import javax.inject.Inject
import javax.inject.Singleton
import play.api.mvc.AbstractController
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.ControllerComponents
import play.api.mvc.Request
import play.api.libs.json.Json

import services.HelloService

@Singleton
class HelloController @Inject()(cc: ControllerComponents, hello: HelloService) extends AbstractController(cc) {

  def get(name: Option[String]) =
    Action { implicit request: Request[AnyContent] =>
      Ok {
        // name
        //   .map(s => s"Hello, $s!")
        //   .getOrElse("""Please give a name as a query parameter named "name".""")
        Json.toJson(HelloResponse(s"Hello, ${name.getOrElse("NONAME")}!!! - ${hello.item(8863)}", 200))
      }
    }
}
