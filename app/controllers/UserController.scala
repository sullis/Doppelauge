package controllers

/* !! This is an example controller and not included in the package !! */


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

class UserController extends Controller {

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
      200: User
      404: Any <- User not found
      400: User <- Syntax error

    User: controllers.User(-type_,+type)
      id: String
      data: UserData(optional)
      type: Enum(man, woman, cat) String(optional)

    UserData: controllers.UserData
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

    RESULT
      200: Array User
      404: String <- User not found
      400: Int <- Syntax Error

  """)
  def get2()  = Action { request =>
    Ok(J.arr(user.toJson).asJsValue)
  }

  @ApiDoc(doc="""
    GET /user2admin/api/v1/guser

    DESCRIPTION
      Get an array of gusers
  """)
  def get3()  = Action { request =>
    Ok(J.arr(user.toJson).asJsValue)
  }

  @ApiDoc(doc="""
    GET /user2admin/api/v1/auser

    DESCRIPTION
      Get an array of ausers
  """)
  def get4()  = Action { request =>
    Ok(J.arr(user.toJson).asJsValue)
  }

  @ApiDoc(doc="""
    POST /user2admin/api/v1/guser/{id_user}

    DESCRIPTION
      Create a user
      You can add more detailed information here.

    PARAMETERS
      body: User
      id_user: Long

    RESULT
      203: User
      404: User <- User not found
      400: UserData <- Syntax Error
  """)
  def post2(id_user: Long)  = Action { request =>
    val json = J(request.body.asJson.get)
    user = User(
      id_user.toString,
      UserData(
        json("data")("firstName").asString,
        json("data")("lastName").asString
      ),
      json("type").asString
    )
    Ok(user.toJson.asJsValue)
  }

/*
  @ApiDoc(doc="""
    POST /user2admin/api/v1/huser/{id_user}

    DESCRIPTION
      Create a user
      You can add more detailed information here.

    PARAMETERS
      body: User
      id_user: Long

    RESULT
      203: User
      404: User <- User not found
      400: UserData <- Syntax Error
  """)
  def post3(id_user: Long)  = Action { request =>
    val json = J(request.body.asJson.get)
    user = User(
      id_user.toString,
      UserData(
        json("data")("firstName").asString,
        json("data")("lastName").asString
      ),
      json("type").asString
    )
    Ok(user.toJson.asJsValue)
  }
 */
}
