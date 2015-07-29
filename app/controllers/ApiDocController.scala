package no.samordnaopptak.apidoc.controllers


import play.api.mvc._

import no.samordnaopptak.apidoc.{AnnotationHelper, SwaggerUtil, ApiDoc}
import no.samordnaopptak.apidoc.{RoutesHelper, RouteEntry}


class ApiDocController extends Controller {

/*
  private val AccessControlAllowOrigin = ("Access-Control-Allow-Origin", "*")
  private val AccessControlAllowMethods = ("Access-Control-Allow-Methods", "GET, POST, DELETE, PUT")
  private val AccessControlAllowHeaders = ("Access-Control-Allow-Headers", "Content-Type");
 */
  

  @ApiDoc(doc="""
    GET /api/v1/api-docs

    DESCRIPTION
      Get main swagger json documentation
      You can add more detailed information here.
  """)
  def get(routeEntries: List[RouteEntry] = RoutesHelper.getRouteEntries()) = {
    val apidocs = AnnotationHelper.getApiDocsFromAnnotations(routeEntries)
    Ok(SwaggerUtil.getMain("/", apidocs).asJsValue)
  }
}


object ApiDocController extends Controller {
  val controller = new ApiDocController

  def get()  = Action { request =>
    controller.get()

    // When extending:
    // 1. copy jsonstring from SwaggerSpec.scala in here
    // 2. uncomment line below (i.e. "Ok(Json.parse(jsonstring))")
    // 3. extend jsonstring with new functionality
    // 4. copy jsonstring back
    // 5. fix code add SwaggerTestData.apidocstrings so that the tests pass
    // 6. comment the line below again
    // 7. done.
    //Ok(Json.parse(jsonstring))
  }

  def validate(routeEntries: List[RouteEntry] = RoutesHelper.getRouteEntries()) = {
    AnnotationHelper.validate(routeEntries)
    controller.get(routeEntries)
  }
}
