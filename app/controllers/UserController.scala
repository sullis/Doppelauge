package no.samordnaopptak.apidoc.controllers


import play.api.mvc._
import play.api.libs.json._
import play.api.Play.current

import no.samordnaopptak.apidoc.{ApiDoc, ApiDocUtil, SwaggerUtil}

case class User(id: String, data: UserData)
case class UserData(firstName: String, lastName: String)


object UserController extends Controller {
  val controller = new ApiDocController

  private val AccessControlAllowOrigin = ("Access-Control-Allow-Origin", "*")

  val user = User("1235f",UserData("CommonFirstName", "CommonLastName"))

  @ApiDoc(doc="""
    GET /useradmin/api/v1/user

    DESCRIPTION
      Get user
      You can add more detailed information here.

    RESULT
      User

    User: no.samordnaopptak.apidoc.controllers.User
      id: String
      data: UserData

    UserData: no.samordnaopptak.apidoc.controllers.UserData
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
    )).withHeaders(AccessControlAllowOrigin)
  }

  @ApiDoc(doc="""
    GET /user2admin/api/v1/user

    DESCRIPTION
      Get user
      You can add more detailed information here.

    RESULT
      User
  """)
  def get2()  = Action { request =>
    Ok(Json.obj(
      "id" -> user.id,
      "data" -> Json.obj(
        "firstName" -> user.data.firstName,
        "lastName" -> user.data.lastName
      )
    )).withHeaders(AccessControlAllowOrigin)
  }
}
