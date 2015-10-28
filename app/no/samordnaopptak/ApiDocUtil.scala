package no.samordnaopptak.apidoc

import no.samordnaopptak.json._


/**
  * High level interface to ApiDoc
  */
object ApiDocUtil{

  /**
    * @return swagger 2.0 docs.
    * 
    * Implementation:
    * {{{
  def getSwaggerDocs(basePath: String = "/", apidocstrings: List[String] = AnnotationHelper.getApiDocsFromAnnotations()): JObject = {
    val apiDocs = ApiDocParser.getApiDocs(apidocstrings)
    val dataTypes = ApiDocParser.getDataTypes(apidocstrings)

    SwaggerUtil.getMain(basePath, apiDocs, dataTypes)
  }
    * }}}
    */
  def getSwaggerDocs(basePath: String = "/", apidocstrings: List[String] = AnnotationHelper.getApiDocsFromAnnotations()): JObject = {
    val apiDocs = ApiDocParser.getApiDocs(apidocstrings)
    val dataTypes = ApiDocParser.getDataTypes(apidocstrings)

    SwaggerUtil.getMain(basePath, apiDocs, dataTypes)
  }

  /**
    * Main validation function. Should be called from tests.
    * @see [[https://github.com/sun-opsys/Doppelauge/blob/master/API_DOC.md#fixing-runtime-exceptions API_DOC.md ]] for instructions on how to handle runtime exceptions.
    */
  def validate(basePath: String = "/", routeEntries: List[RouteEntry] = RoutesHelper.getRouteEntries()): Unit = {
    AnnotationHelper.validate(routeEntries)
    ApiDocUtil.getSwaggerDocs(basePath)
  }
}
