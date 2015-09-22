package no.samordnaopptak.apidoc

import no.samordnaopptak.json._


object ApiDocUtil{

  def getSwaggerDocs(basePath: String = "/", apidocstrings: List[String] = AnnotationHelper.getApiDocsFromAnnotations()): JObject = {
    SwaggerUtil.getMain(basePath, apidocstrings)
  }

  def validate(routeEntries: List[RouteEntry] = RoutesHelper.getRouteEntries()): Unit = {
    AnnotationHelper.validate(routeEntries)
  }
}
