package no.samordnaopptak.apidoc.controllers


import play.api.mvc._
import play.api.libs.json._
import play.api.Play.current

import no.samordnaopptak.apidoc.TestByAnnotation.Test

import no.samordnaopptak.apidoc.{ApiDoc, ApiDocUtil, SwaggerUtil}
import no.samordnaopptak.apidoc.{RoutesHelper, RouteEntry}


class ApiDocController extends Controller {

/*
  private val AccessControlAllowOrigin = ("Access-Control-Allow-Origin", "*")
  private val AccessControlAllowMethods = ("Access-Control-Allow-Methods", "GET, POST, DELETE, PUT")
  private val AccessControlAllowHeaders = ("Access-Control-Allow-Headers", "Content-Type");
 */
  
  @Test(code="""
     self.hasSameUri("/api/v1/acl", "/api/v1/acl")   === true
     self.hasSameUri("/1api/v1/acl", "/api/v1/acl")  =/= true
     self.hasSameUri("/api/v1/acl", "/api/v1/acl" )  === true
     self.hasSameUri("/1api/v1/acl", "/api/v1/acl" ) =/= true
     self.hasSameUri("/api/v1/acl", "/api/v1/acl2" ) =/= true
     self.hasSameUri("/api/v1/acl", "/api/v1"      ) =/= true

     self.hasSameUri("/1api/v1/acl/{service}", "/api/v1/acl/$service<[^/]+>") =/= true

     self.hasSameUri("/api/v1/acl/{service}", "/api/v1/acl/$service<[^/]+>" ) === true

     self.hasSameUri("/api/v1/acl",          "/api/v1/acl/$service<[^/]+> " ) =/= true
     self.hasSameUri("/api/v1/acl",          "/api/v1/acl/$service<[^/]+> " ) =/= true

     self.hasSameUri("/api/v1/acl",          "/api/v1/acl"                  ) === true
     self.hasSameUri("/api/v1/acl",          "/api/v1/acl/"                 ) === true
     self.hasSameUri("/api/v1/acl",          "/api/v1/acl"                  ) === true
     self.hasSameUri("/api/v1/acl",          "/api/v1/acl/"                 ) === true
     self.hasSameUri("/api/v1/acl/{service}", "/api/v1/acl/"                ) =/= true
     self.hasSameUri("/api/v1/acl/{service}", "/api/v1/acl"                 ) =/= true

      // one parameter in the middle of the uri:
     self.hasSameUri("/api/v1/acl/{service}/hepp", "/api/v1/acl/$service<[^/]+>/hepp" ) === true
     self.hasSameUri("/api/v1/acl/{service}/hepp/", "/api/v1/acl/$service<[^/]+>/hepp/" ) === true
     self.hasSameUri("/api/v1/acl/{service}/hepp", "/api/v1/acl/$service<[^/]+>/hepp/" ) === true
     self.hasSameUri("/api/v1/acl/{service}/hepp/", "/api/v1/acl/$service<[^/]+>/hepp" ) === true

     self.hasSameUri("/api/v1/acl/{service}/hepp2", "/api/v1/acl/$service<[^/]+>/hepp" ) === false
     self.hasSameUri("/api/v1/acl/{service2}/hepp", "/api/v1/acl/$service<[^/]+>/hepp" ) === false
     self.hasSameUri("/api/v1/acl2/{service}/hepp", "/api/v1/acl/$service<[^/]+>/hepp" ) === false
     self.hasSameUri("/2api/v1/acl/{service}/hepp", "/api/v1/acl/$service<[^/]+>/hepp" ) === false
     self.hasSameUri("/api/v1/acl/{service}/hepp", "/api/v1/acl/$service<[^/]+>/hepp2" ) === false
     self.hasSameUri("/api/v1/acl/{service}/hepp", "/api/v1/acl/$service<[^/]+>/hepp/2" ) === false
     self.hasSameUri("/api/v1/acl/{service}/hepp", "/api/v1/acl/$service2<[^/]+>/hepp/" ) === false
     self.hasSameUri("/api/v1/acl/{service}/hepp", "/api/v1/acl2/$service<[^/]+>/hepp/" ) === false
     self.hasSameUri("/api/v1/acl/{service}/hepp", "/api2/v1/acl/$service<[^/]+>/hepp/" ) === false

     // two parameters in the middle of the uri:
     self.hasSameUri("/api/v1/acl/{service}/{hepp}", "/api/v1/acl/$service<[^/]+>/$hepp<[^/]+>" ) === true
     self.hasSameUri("/api/v1/acl/{service}/gakk/{hepp}", "/api/v1/acl/$service<[^/]+>/gakk/$hepp<[^/]+>" ) === true
  """)
  def hasSameUri(autoUri: String, confUri: String): Boolean = {
    val autos = autoUri.split("/")
    val confs = RoutesHelper.getAutoUriFromConfUri(confUri).split("/")

    autos.size == confs.size &&
    autos.zip(confs).forall(g => {
      val auto = g._1
      val conf = g._2
      auto == conf
    })
  }

  class MethodMismatchException(val message: String) extends Exception(message)
  class UriMismatchException(val message: String) extends Exception(message)

  def hasMethodAnnotation(className: String, methodName: String) = {
    val class_ = play.api.Play.classloader.loadClass(className)
    class_.getDeclaredMethods().find(_.getName()==methodName) match {
      case None => false
      case Some(method) => {
        val annotations = method.getAnnotations()
        annotations.find(_.isInstanceOf[no.samordnaopptak.apidoc.ApiDoc]) != None
      }
    }
  }

  def getMethodAnnotation(className: String, methodName: String) = {
    val class_ = play.api.Play.classloader.loadClass(className)
    class_.getDeclaredMethods().find(
      _.getName()==methodName
    ).get.getAnnotations().find(
      _.isInstanceOf[no.samordnaopptak.apidoc.ApiDoc]
    ).get.asInstanceOf[no.samordnaopptak.apidoc.ApiDoc]
  }

  def validate(routeEntries: List[RouteEntry]): Unit = {
    routeEntries.foreach(routeEntry => {
      //println("Validating "+routeEntry)

      if (!hasMethodAnnotation(routeEntry.scalaClass, routeEntry.scalaMethod))
        throw new Exception(s"Missing ApiDoc for ${routeEntry.scalaClass}.${routeEntry.scalaMethod} (Make sure the Class is annotated, and not the corresponding Object)")

      val annotation = getMethodAnnotation(routeEntry.scalaClass, routeEntry.scalaMethod)
      val json = ApiDocUtil.getJson(annotation.doc)
      val jsonMethod = (json\"method").as[String]
      val jsonUri = (json\"uri").as[String]

      if (jsonMethod != routeEntry.restMethod)
        throw new MethodMismatchException(s"Conflicting REST method declared in the autodoc and in conf/routes for ${routeEntry.scalaClass}.${routeEntry.scalaMethod}.\n"+
                                          s"autodoc: $jsonMethod. conf/routes: ${routeEntry.restMethod}")

      if (!hasSameUri(jsonUri, routeEntry.uri))
        throw new UriMismatchException(s"Conflicting uri declared in the autodoc and in conf/routes for ${routeEntry.scalaClass}.${routeEntry.scalaMethod}.\n"+
                                       s"autodoc: $jsonUri. conf/routes: ${routeEntry.uri}")
    })
  }

  def getApiDocsFromAnnotations(routeEntries: List[RouteEntry] = RoutesHelper.getRouteEntries()): List[String] = {

    validate(routeEntries)

    val apiDocAnnotations = routeEntries.map(routeEntry =>
      getMethodAnnotation(routeEntry.scalaClass, routeEntry.scalaMethod)
    )

    val apiDocs = apiDocAnnotations.map(_.doc)

    //println("apiDocs: "+apiDocs)

    apiDocs
  }

  @ApiDoc(doc="""
    GET /api/v1/api-docs

    DESCRIPTION
      Get main swagger json documentation
      You can add more detailed information here.
  """)
  def get(routeEntries: List[RouteEntry] = RoutesHelper.getRouteEntries()) = {
    Ok(SwaggerUtil.getMain("/", getApiDocsFromAnnotations(routeEntries)))
  }
}


object ApiDocController extends Controller {
  val controller = new ApiDocController

  def get()  = Action { request =>
    val routeEntries =
      RoutesHelper.getRouteEntries()
        .filter(_.scalaClass != "controllers.Assets") // no api-doc for the static assets files

    controller.get(routeEntries)

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

  def validate(routeEntries: List[RouteEntry] = RoutesHelper.getRouteEntries()) =
    controller.get(routeEntries)
}
