package test

import play.api.Play.current

import org.specs2.mutable._

import controllers.routes

import play.api.test._
import play.api.test.Helpers._
import play.api.libs.json._

import controllers.ApiDocController
import no.samordnaopptak.apidoc.JsonMatcher._
import no.samordnaopptak.apidoc.{ApiDoc, SwaggerUtil}


class ApiDocControllerSpec extends Specification {

  @ApiDoc(doc="""
    GET /api/v1/users/{id}

    DESCRIPTION
      Get user

    PARAMETERS 
      id: String <- ID of the user

    ERRORS
      400 User not found
      400 Syntax Error

    RESULT
      User
  """)
  def errorDoc() = true

  @ApiDoc(doc="""
    GET /api/v1/users/

    DESCRIPTION
      Get user

    PARAMETERS 
      id: String (header) <- ID of the user

    ERRORS
      400 User not found
      400 Syntax Error

    RESULT
      User
  """)
  def errorDoc2() = true

  def inCleanEnvironment()(func: => Unit): Boolean = {
    running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      func
    }
    true
  }

  def getFutureResult(result: play.api.mvc.SimpleResult) =
    play.api.mvc.Action { result } (FakeRequest())

  def checkResult(
    result: play.api.mvc.SimpleResult,
    matcher: JsValue,
    statusCode: Int = OK,
    mustNotContain: List[String] = List()
  ){
    val futureResult = getFutureResult(result)
    contentType(futureResult) must beSome.which(_ == "application/json")

    val json: JsValue = contentAsJson(futureResult)
    matchJson(matcher, json)

    mustNotContain.foreach(contentAsString(futureResult) must not contain(_))
  }


  "ApiDoc controller" should {

    "Check that the hasSameUri function works" in {
      val controller = new ApiDocController
      controller.hasSameUri("/api/v1/acl", "/api/v1/acl" )must beTrue
      controller.hasSameUri("/1api/v1/acl", "/api/v1/acl" )must beFalse
      controller.hasSameUri("/api/v1/acl", "/api/v1/acl2" )must beFalse
      controller.hasSameUri("/api/v1/acl", "/api/v1" )must beFalse
      controller.hasSameUri("/1api/v1/acl/{service}", "/api/v1/acl/$service<[^/]+>" )must beFalse

      controller.hasSameUri("/api/v1/acl/{service}", "/api/v1/acl/$service<[^/]+>" )must beTrue
      controller.hasSameUri("/api/v1/acl",          "/api/v1/acl/$service<[^/]+>" )must beFalse
      controller.hasSameUri("/api/v1/acl",          "/api/v1/acl/$service<[^/]+>" )must beFalse
      controller.hasSameUri("/api/v1/acl",          "/api/v1/acl"                 )must beTrue
      controller.hasSameUri("/api/v1/acl",          "/api/v1/acl/"                )must beTrue
      controller.hasSameUri("/api/v1/acl",          "/api/v1/acl"                 )must beTrue
      controller.hasSameUri("/api/v1/acl",          "/api/v1/acl/"                )must beTrue
      controller.hasSameUri("/api/v1/acl/{service}", "/api/v1/acl/"                )must beFalse
      controller.hasSameUri("/api/v1/acl/{service}", "/api/v1/acl"                 )must beFalse

      // one parameter in the middle of the uri:

      controller.hasSameUri("/api/v1/acl/{service}/hepp", "/api/v1/acl/$service<[^/]+>/hepp" )must beTrue
      controller.hasSameUri("/api/v1/acl/{service}/hepp/", "/api/v1/acl/$service<[^/]+>/hepp/" )must beTrue
      controller.hasSameUri("/api/v1/acl/{service}/hepp", "/api/v1/acl/$service<[^/]+>/hepp/" )must beTrue
      controller.hasSameUri("/api/v1/acl/{service}/hepp/", "/api/v1/acl/$service<[^/]+>/hepp" )must beTrue

      controller.hasSameUri("/api/v1/acl/{service}/hepp2", "/api/v1/acl/$service<[^/]+>/hepp" )must beFalse
      controller.hasSameUri("/api/v1/acl/{service2}/hepp", "/api/v1/acl/$service<[^/]+>/hepp" )must beFalse
      controller.hasSameUri("/api/v1/acl2/{service}/hepp", "/api/v1/acl/$service<[^/]+>/hepp" )must beFalse
      controller.hasSameUri("/2api/v1/acl/{service}/hepp", "/api/v1/acl/$service<[^/]+>/hepp" )must beFalse
      controller.hasSameUri("/api/v1/acl/{service}/hepp", "/api/v1/acl/$service<[^/]+>/hepp2" )must beFalse
      controller.hasSameUri("/api/v1/acl/{service}/hepp", "/api/v1/acl/$service<[^/]+>/hepp/2" )must beFalse
      controller.hasSameUri("/api/v1/acl/{service}/hepp", "/api/v1/acl/$service2<[^/]+>/hepp/" )must beFalse
      controller.hasSameUri("/api/v1/acl/{service}/hepp", "/api/v1/acl2/$service<[^/]+>/hepp/" )must beFalse
      controller.hasSameUri("/api/v1/acl/{service}/hepp", "/api2/v1/acl/$service<[^/]+>/hepp/" )must beFalse

      // two parameters in the middle of the uri:
      controller.hasSameUri("/api/v1/acl/{service}/{hepp}", "/api/v1/acl/$service<[^/]+>/$hepp<[^/]+>" )must beTrue
      controller.hasSameUri("/api/v1/acl/{service}/gakk/{hepp}", "/api/v1/acl/$service<[^/]+>/gakk/$hepp<[^/]+>" )must beTrue
    }


    "validate that the validate method that validates if method and uri in conf/routes and autodoc matches works" in {
      inCleanEnvironment() {
        val controller = new ApiDocController

        val routeEntries = controller.getRouteEntries()
        controller.validate(routeEntries)

        {
          val validRouteEntry = controller.RouteEntry("GET", "/api/v1/users/$id<[^/]+>", "test.ApiDocControllerSpec", "errorDoc")
          controller.validate(validRouteEntry::routeEntries)
        }

        {
          val errorRouteEntry = controller.RouteEntry("GET", "/api/v1/flapp", "test.ApiDocControllerSpec", "errorDoc")
          controller.validate(errorRouteEntry::routeEntries) should throwA[controller.UriMismatchException]
        }

        {
          val errorRouteEntry = controller.RouteEntry("PUT", "/api/v1/users/$id<[^/]+>", "test.ApiDocControllerSpec", "errorDoc")
          controller.validate(errorRouteEntry::routeEntries) should throwA[controller.MethodMismatchException]
        }

        {
          val errorRouteEntry = controller.RouteEntry("GET", "/api/v1/users", "test.ApiDocControllerSpec", "errorDoc")
          controller.validate(errorRouteEntry::routeEntries) should throwA[controller.UriMismatchException]
        }

        {
          val errorRouteEntry = controller.RouteEntry("GET", "/api/v1/users/$id<[^/]+>", "test.ApiDocControllerSpec", "errorDoc2")
          controller.validate(errorRouteEntry::routeEntries) should throwA[controller.UriMismatchException]
        }
      }
    }

    "Call Swagger.getJson on all annotated resource path groups, in order to run validation checks on them" in {
      inCleanEnvironment() {
        ApiDocController.validate("/api/v1/")
      }
    }

    "return main swagger menu" in {
      inCleanEnvironment() {

        val controller = new ApiDocController

        checkResult(
          controller.get(),
          Json.obj(
            "info" -> Json.obj(___allowOtherFields),
            "apis" -> Json.arr(
              Json.obj(
                "path" -> "/api-docs",
                "description" -> "Operations on api-docs"),
              ___allowOtherValues
            ),
            ___allowOtherFields
          )
        )
      }
    }

    "return users api doc" in {
      inCleanEnvironment() {

        val controller = new ApiDocController

        checkResult(
          controller.getPath("api-docs"),
          Json.obj(
            "models" -> Json.obj(___allowOtherFields),
            "resourcePath" -> "/api-docs",
            "apis" -> Json.arr(
              Json.obj(
                "operations" -> Json.arr(Json.obj(
                  "method" -> "GET",
                  ___numElements -> 7)),
                "path" -> "/../../../api/v1/api-docs"),
              Json.obj(
                "operations" -> Json.arr(Json.obj(
                  "method" -> "GET",
                  ___numElements -> 7)),
                "path" -> "/../../../api/v1/api-docs/{path}")
            ),
            ___allowOtherFields
          ))
      }
    }
  }
}

