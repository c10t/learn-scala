package controllers

import play.api.libs.json.Json
import play.api.libs.json.Writes

case class HelloResponse(message: String, status: Int)

object HelloResponse {
  implicit val writes: Writes[HelloResponse] = new Writes[HelloResponse] {
    def writes(res: HelloResponse) = Json.obj("msg" -> res.message, "status" -> res.status)
  }
}
