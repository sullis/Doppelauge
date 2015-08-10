package no.samordnaopptak.apidoc.controllers


import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import play.api.mvc._
import play.api.Play.current

import no.samordnaopptak.apidoc.ApiDoc
import no.samordnaopptak.json._


case class UserData(firstName: String, lastName: String){
  def toJson = J.obj(
    "firstName" -> firstName,
    "lastName" -> lastName
  )
}

case class User(id: String, data: UserData, type_ : String){
  def toJson = J.obj(
    "id" -> id,
    "data" -> data.toJson,
    "type" -> type_
  )
}

object UserController extends Controller {
  val controller = new ApiDocController

  /*
  private val AccessControlAllowOrigin = ("Access-Control-Allow-Origin", "*")
   */

  var user = User("1235f",UserData("CommonFirstName", "CommonLastName"), "cat")

  @ApiDoc(doc="""
    GET /useradmin/api/v1/user

    DESCRIPTION
      Get user
      You can add more detailed information here.
      _ Use underscrores to
      __ indent lines

    PARAMETERS
      offset: Int (query, optional) <- default = 0
      limit: Int (query) <- default = 0
      enumParameterExample: Enum(4,5,6,7) Int(query) <- enum value

    RESULT
      User

    ERRORS
      404 User not found
      400 Syntax Error

    User: no.samordnaopptak.apidoc.controllers.User(-type_,+type)
      id: String
      data: UserData(optional)
      type: Enum(man, woman, cat) String(optional)

    UserData: no.samordnaopptak.apidoc.controllers.UserData
      firstName: String
      lastName: String
  """)
  def get()  = Action { request =>
    Ok(user.toJson.asJsValue)
  }

  @ApiDoc(doc="""
    GET /user2admin/api/v1/user

    DESCRIPTION
      Get an array of users
      You can add more detailed information here.

    ERRORS
      404 User not found
      400 Syntax Error

    RESULT
      Array User
  """)
  def get2()  = Action { request =>
    Ok(J.arr(user.toJson).asJsValue)
  }

  @ApiDoc(doc="""
    POST /user2admin/api/v1/guser

    DESCRIPTION
      Create a user
      You can add more detailed information here.

    PARAMETERS
      body: User

    ERRORS
      404 User not found
      400 Syntax Error

    RESULT
      User
  """)
  def post2()  = Action { request =>
    val json = J(request.body.asJson.get)
    user = User(
      json("id").asString,
      UserData(
        json("data")("firstName").asString,
        json("data")("lastName").asString
      ),
      json("type").asString
    )
    Ok(user.toJson.asJsValue)
  }

}
