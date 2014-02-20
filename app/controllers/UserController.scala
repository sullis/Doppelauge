package controllers


import play.api.mvc._
import play.api.libs.json._
import play.api.Play.current

import no.samordnaopptak.apidoc.{ApiDoc, ApiDocUtil, SwaggerUtil}

case class User(id: String, data: UserData)
case class UserData(firstName: String, lastName: String)


object UserController extends Controller {
  val controller = new ApiDocController

  val user = User("1235f",UserData("CommonFirstName", "CommonLastName"))

  @ApiDoc(doc="""
    GET /api/v1/user

    DESCRIPTION
      Get main swagger json documentation

    RESULT
      User

    User: controllers.User
      id: String
      data: UserData

    UserData: controllers.UserData
      firstName: String
      lastName: String
  """)
  def get()  = Action { request =>
    Ok(Json.obj(
      "id" -> user.id,
      "data" -> Json.obj(
        "firstName" -> user.data.firstName,
        "lastName" -> user.data.lastName
      )
    ))
  }
}
