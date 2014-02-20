package controllers


import play.api.mvc._
import play.api.libs.json._
import play.api.Play.current

import no.samordnaopptak.apidoc.{ApiDoc, ApiDocUtil, SwaggerUtil}


class ApiDocController extends Controller {

  case class RouteEntry(restMethod: String, uri: String, scalaClass: String, scalaMethod: String)

  // code in this method copied from the swagger play2 module.
  private def getRestClassName(annoDocField3: String) = {
    val m1 = annoDocField3.lastIndexOf("(") match {
      case i: Int if (i > 0) => annoDocField3.substring(0, i)
      case _ => annoDocField3
    }
    m1.substring(0, m1.lastIndexOf(".")).replace("@", "")
  }

  //controllers.ApiDocController.get -> get
  //controllers.ApiDocController.get(path:String) -> get
  private def getRestMethodName(annoDocField3: String) =
    if (annoDocField3.endsWith(")")) {
      val last = annoDocField3.lastIndexOf("(")
      val s2 = annoDocField3.take(last)
      val secondLast = s2.lastIndexOf(".")
      //println("annodoc: "+annoDocField3+", "+secondLast+", "+last+", "+s2.take(secondLast))
      s2.drop(secondLast+1)
    } else {
      val last = annoDocField3.lastIndexOf(".")
      annoDocField3.drop(last+1)
    }


  def getRouteEntries(): List[RouteEntry] =
    play.api.Play.routes.get.documentation.map(doc =>
      RouteEntry(doc._1, doc._2, getRestClassName(doc._3), getRestMethodName(doc._3))
    ).filter(routeEntry =>
      routeEntry.scalaClass != "controllers.Assets" &&
      routeEntry.scalaClass != "controllers.StaticFile" &&
      routeEntry.scalaClass != "controllers.Application"
    ).toList

  // "/api/v1/acl"                 -> /api/v1/acl
  // "/api/v1/acl/"                -> /api/v1/acl
  // "/api/v1/acl/$service<[^/]+>" -> /api/v1/acl
  def findConfUriBase(confUri: String) = {
    if (confUri.endsWith(">")) {
      val last = confUri.lastIndexOf('$')
      confUri.take(last-1)
    } else if (confUri.endsWith("/"))
      confUri.dropRight(1)
    else
      confUri
  }

  // "/api/v1/acl/$service<[^/]+>" -> "service"
  // "/api/v1/acl/",         -> ""
  // "/api/v1/acl",          -> ""
  def findConfUriParm(confUri: String) = {
    if (confUri.endsWith(">")) {
      val last = confUri.lastIndexOf('<')
      val secondLast = confUri.lastIndexOf('$')
      //println("conf: "+confUri+", "+secondLast+", "+last)
      confUri.substring(secondLast+1, last)
    } else
      ""
  }

  // "/api/v1/acl/{service}", "/api/v1/acl/$service<[^/]+>" -> true
  // "/api/v1/acl/",          "/api/v1/acl/$service<[^/]+>" -> false
  // "/api/v1/acl",           "/api/v1/acl/$service<[^/]+>" -> false
  // "/api/v1/acl",           "/api/v1/acl"                 -> true
  // "/api/v1/acl",           "/api/v1/acl/"                -> true
  // "/api/v1/acl/",          "/api/v1/acl"                 -> true
  // "/api/v1/acl/",          "/api/v1/acl/"                -> true
  // "/api/v1/acl/{service}", "/api/v1/acl/"                -> false
  // "/api/v1/acl/{service}", "/api/v1/acl"                 -> false
  def hasSameUri(autoUriBase: String, autoUriParm: String, confUri: String): Boolean = {
    autoUriBase == findConfUriBase(confUri) &&
    autoUriParm == findConfUriParm(confUri)
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
      val jsonUriBase = (json\"uriBase").as[String]
      val jsonUriParm = (json\"uriParm").as[String]

      if (jsonMethod != routeEntry.restMethod)
        throw new MethodMismatchException(s"Conflicting REST method declared in the autodoc and in conf/routes for ${routeEntry.scalaClass}.${routeEntry.scalaMethod}.\n"+
                                          s"autodoc: $jsonMethod. conf/routes: ${routeEntry.restMethod}")

      if (!hasSameUri(jsonUriBase, jsonUriParm, routeEntry.uri))
        throw new UriMismatchException(s"Conflicting uri declared in the autodoc and in conf/routes for ${routeEntry.scalaClass}.${routeEntry.scalaMethod}.\n"+
                                       s"autodoc: $jsonUri. conf/routes: ${routeEntry.uri}")
    })
  }

  lazy val apiDocsFromAnnotations: List[String] = {

    val routeEntries = getRouteEntries()

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
  """)
  def get() = {
    Ok(SwaggerUtil.getMain("/api/v1/", apiDocsFromAnnotations))
  }

  @ApiDoc(doc="""
    GET /api/v1/api-docs/{path}

    DESCRIPTION
      Get swagger documentation json for a resource path

    PARAMETERS
      path: String
  """)
  def getPath(path: String) = {
    Ok(SwaggerUtil.getJson("/api/v1/", apiDocsFromAnnotations, path))
  }
}

object ApiDocController extends Controller {
  val controller = new ApiDocController

  def get()  = Action { request =>
    controller.get()
  }
  def getPath(path: String)  = Action { request =>
    controller.getPath(path)
  }

  def validate(basePath: String) = {
    val annotations = controller.apiDocsFromAnnotations
    SwaggerUtil.allResourcePathGroups(basePath, annotations).foreach(
      SwaggerUtil.getJson(basePath, annotations, _)
    )
  }
}
