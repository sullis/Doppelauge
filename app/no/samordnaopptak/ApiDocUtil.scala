package no.samordnaopptak.apidoc

import no.samordnaopptak.json._


object ApiDocUtil{

  /**
    * Main function to get swagger docs
    */
  def getSwaggerDocs(basePath: String = "/", apidocstrings: List[String] = AnnotationHelper.getApiDocsFromAnnotations()): JObject = {
    SwaggerUtil.getMain(basePath, apidocstrings)
  }

  /**
    * Main validation function. Should be called from tests.
    */
  def validate(routeEntries: List[RouteEntry] = RoutesHelper.getRouteEntries()): Unit = {
    AnnotationHelper.validate(routeEntries)
  }
}
