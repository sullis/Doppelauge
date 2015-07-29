package test

import play.api.Play.current

import org.specs2.mutable._

import no.samordnaopptak.apidoc.controllers.routes

import play.api.test._
import play.api.test.Helpers._
import play.api.libs.json._

import no.samordnaopptak.json.JsonMatcher._

import no.samordnaopptak.apidoc.controllers.ApiDocController
import no.samordnaopptak.apidoc.{RoutesHelper, RouteEntry}



class ApiDocControllerSpec extends Specification {

  def inCleanEnvironment()(func: => Unit): Boolean = {
    running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      func
    }
    true
  }

  def getFutureResult(result: play.api.mvc.Result) =
    play.api.mvc.Action { result } (FakeRequest())

  def checkResult(
    result: play.api.mvc.Result,
    matcher: JsValue,
    statusCode: Int = OK,
    mustNotContain: List[String] = List()
  ){
    val futureResult = getFutureResult(result)

    if (contentType(futureResult) != Some("application/json"))
      throw new Exception("contentType(futureResult) is not Some(\"application/json\"), but "+contentType(futureResult))

    val json: JsValue = contentAsJson(futureResult)
    matchJson(matcher, json)

    mustNotContain.foreach(contentAsString(futureResult) must not contain(_))
  }


  "ApiDoc controller" should {

    def routeEntries =
      RoutesHelper.getRouteEntries()
        .filter(_.scalaClass != "controllers.Assets") // no api-doc for the static assets files


    "return main swagger menu" in {
      inCleanEnvironment() {

        val controller = new ApiDocController

        checkResult(
          controller.get(routeEntries),
          Json.obj(
            "paths" -> Json.obj(
              ___allowOtherFields
            ),
            ___allowOtherFields
          )
        )
      }
    }

  }
}

