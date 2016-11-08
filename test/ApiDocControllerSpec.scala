package test

import play.api.Play.current

import org.specs2.mutable._

import com.google.inject.Inject

import controllers.routes

import play.api.test._
import play.api.test.Helpers._

import no.samordnaopptak.json._

import no.samordnaopptak.apidoc.{RoutesHelper, ApiDocUtil}



class ApiDocControllerSpec @Inject()(apiDocUtil: ApiDocUtil)  extends Specification {

  def inCleanEnvironment(func: => Unit): Boolean = {
    running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      func
    }
    true
  }

  def checkResult(
    call: => play.api.mvc.Call,
    matcher: JValue,
    statusCode: Int = OK,
    mustNotContain: List[String] = List()
  ){

    val request = FakeRequest(
      call.method,
      call.url,
      FakeHeaders(),
      play.api.mvc.AnyContentAsEmpty
    )

    val result = route(request).get

    if (contentType(result) != Some("application/json"))
      throw new Exception("contentType(result) is not Some(\"application/json\"), but "+contentType(result))

    val json = contentAsJson(result)

    JsonMatcher.matchJson(matcher, json)

    mustNotContain.foreach(contentAsString(result) must not contain(_))
  }


  "ApiDoc controller" should {

    def routeEntries =
      RoutesHelper.getRouteEntries()
        .filter(_.scalaClass != "controllers.Assets") // no api-doc for the static assets files


    "return main swagger menu" in {
      inCleanEnvironment {

        checkResult(
          routes.ApiDocController.get,
          J.obj(
            "paths" -> J.obj(
              JsonMatcher.___allowOtherFields
            ),
            JsonMatcher.___allowOtherFields
          )
        )
      }
    }

    "Validate swagger api docs" in {
      inCleanEnvironment {
        apiDocUtil.validate(routeEntries = routeEntries)
      }
    }

  }
}

