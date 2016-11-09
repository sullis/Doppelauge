package no.samordnaopptak.apidoc

import play.api.Play.current

import com.google.inject.Inject

import no.samordnaopptak.test.TestByAnnotation.Test


class AnnotationHelper @Inject() (
  environment: play.api.Environment,
  apiDocValidation: ApiDocValidation,
  routesHelper: RoutesHelper
) {

  val apiDocValidator = apiDocValidation

  @Test(code="""
     self.hasSameUri("/api/v1/acl", "/api/v1/acl")   === true
     self.hasSameUri("/1api/v1/acl", "/api/v1/acl")  =/= true
     self.hasSameUri("/api/v1/acl", "/api/v1/acl" )  === true
     self.hasSameUri("/1api/v1/acl", "/api/v1/acl" ) =/= true
     self.hasSameUri("/api/v1/acl", "/api/v1/acl2" ) =/= true
     self.hasSameUri("/api/v1/acl", "/api/v1"      ) =/= true

     self.hasSameUri("/1api/v1/acl/{service}", "/api/v1/acl/{service}") =/= true

     self.hasSameUri("/api/v1/acl/{service}", "/api/v1/acl/{service}" ) === true

     self.hasSameUri("/api/v1/acl",          "/api/v1/acl/{service} " ) =/= true
     self.hasSameUri("/api/v1/acl",          "/api/v1/acl/{service} " ) =/= true

     self.hasSameUri("/api/v1/acl",          "/api/v1/acl"                  ) === true
     self.hasSameUri("/api/v1/acl",          "/api/v1/acl/"                 ) === true
     self.hasSameUri("/api/v1/acl",          "/api/v1/acl"                  ) === true
     self.hasSameUri("/api/v1/acl",          "/api/v1/acl/"                 ) === true
     self.hasSameUri("/api/v1/acl/{service}", "/api/v1/acl/"                ) =/= true
     self.hasSameUri("/api/v1/acl/{service}", "/api/v1/acl"                 ) =/= true

      // one parameter in the middle of the uri:
     self.hasSameUri("/api/v1/acl/{service}/hepp", "/api/v1/acl/{service}/hepp" ) === true
     self.hasSameUri("/api/v1/acl/{service}/hepp/", "/api/v1/acl/{service}/hepp/" ) === true
     self.hasSameUri("/api/v1/acl/{service}/hepp", "/api/v1/acl/{service}/hepp/" ) === true
     self.hasSameUri("/api/v1/acl/{service}/hepp/", "/api/v1/acl/{service}/hepp" ) === true

     self.hasSameUri("/api/v1/acl/{service}/hepp2", "/api/v1/acl/{service}/hepp" ) === false
     self.hasSameUri("/api/v1/acl/{service2}/hepp", "/api/v1/acl/{service}/hepp" ) === false
     self.hasSameUri("/api/v1/acl2/{service}/hepp", "/api/v1/acl/{service}/hepp" ) === false
     self.hasSameUri("/2api/v1/acl/{service}/hepp", "/api/v1/acl/{service}/hepp" ) === false
     self.hasSameUri("/api/v1/acl/{service}/hepp", "/api/v1/acl/{service}/hepp2" ) === false
     self.hasSameUri("/api/v1/acl/{service}/hepp", "/api/v1/acl/{service}/hepp/2" ) === false
     self.hasSameUri("/api/v1/acl/{service}/hepp", "/api/v1/acl/{service2}/hepp/" ) === false
     self.hasSameUri("/api/v1/acl/{service}/hepp", "/api/v1/acl2/{service}/hepp/" ) === false
     self.hasSameUri("/api/v1/acl/{service}/hepp", "/api2/v1/acl/{service}/hepp/" ) === false

     // two parameters in the middle of the uri:
     self.hasSameUri("/api/v1/acl/{service}/{hepp}", "/api/v1/acl/{service}/{hepp}" ) === true
     self.hasSameUri("/api/v1/acl/{service}/gakk/{hepp}", "/api/v1/acl/{service}/gakk/{hepp}" ) === true
  """)
  private def hasSameUri(jsonUri: String, confUri: String): Boolean = {
    val autos = jsonUri.split("/")
    val confs = confUri.split("/")

    autos.size == confs.size &&
    autos.zip(confs).forall(g => {
      val auto = g._1
      val conf = g._2
      auto == conf
    })
  }

  class MissingMethodException(val message: String) extends Exception(message)
  class MethodMismatchException(val message: String) extends Exception(message)
  class UriMismatchException(val message: String) extends Exception(message)


  private def hasAnnotation(method: java.lang.reflect.Method) = {
    val annotations = method.getAnnotations
    annotations.exists(_.isInstanceOf[no.samordnaopptak.apidoc.ApiDoc])
  }


  def hasMethodAnnotation(className: String, methodName: String) = {
    val class_ = environment.classLoader.loadClass(className)

    class_.getDeclaredMethods.exists(
      method => method.getName==methodName && hasAnnotation(method)
    )
  }

  def getMethodAnnotation(className: String, methodName: String) = {
    val class_ = environment.classLoader.loadClass(className)

    val method =
      class_.getDeclaredMethods.find(
        method => method.getName == methodName && hasAnnotation(method)
      ).get

    val rightAnnotation =
      method.getAnnotations.find(
        _.isInstanceOf[no.samordnaopptak.apidoc.ApiDoc]
      ).get

    rightAnnotation.asInstanceOf[no.samordnaopptak.apidoc.ApiDoc]
  }

  private def expandIncludes(doc: String, alreadyIncluded: scala.collection.mutable.Set[String]): String = {
    val lines = doc.split("\n")
    lines.map { line =>
      val trimmed = line.trim

      if (trimmed.startsWith("INCLUDE ")) {

        val pathAndMethod = trimmed.drop("INCLUDE ".length).trim

        if (alreadyIncluded.contains(pathAndMethod)) {

          "\n"

        } else {

          val lastDot = pathAndMethod.lastIndexOf('.')
          val className = pathAndMethod.take(lastDot)
          val methodName = pathAndMethod.drop(lastDot+1)

          /*
           println(line)
           println(pathAndMethod)
           println(className)
           println(methodName)
           */

          alreadyIncluded.add(pathAndMethod)

          getMethodAnnotationDoc(className, methodName, alreadyIncluded)
        }

      } else {

        line + "\n"

      }

    }.mkString
  }

  private def getMethodAnnotationDoc(className: String, methodName: String, alreadyIncluded: scala.collection.mutable.Set[String]) = {
    val annotation = getMethodAnnotation(className, methodName)
    expandIncludes(annotation.doc, alreadyIncluded)
  }

  /**
    * Internal function. Use ApiDocUtil.validate instead.
    */
  def validate(routeEntries: List[RouteEntry]): Unit = {
    val alreadyIncluded = scala.collection.mutable.Set[String]()

    routeEntries.foreach(routeEntry => {

      if (!hasMethodAnnotation(routeEntry.scalaClass, routeEntry.scalaMethod))
        throw new MissingMethodException(s"Missing ApiDoc for ${routeEntry.scalaClass}.${routeEntry.scalaMethod} (Make sure the Class is annotated, and not the companion object) (See README.md for more information)\n")

      val doc = getMethodAnnotationDoc(routeEntry.scalaClass, routeEntry.scalaMethod, alreadyIncluded)

      val apiDoc = ApiDocParser.getApiDoc(apiDocValidation, doc)
      apiDocValidation.validate(apiDoc)
      val json = apiDoc.toJson

      val jsonMethod = json("method").asString
      val jsonUri = json("uri").asString

      if (jsonMethod != routeEntry.restMethod)
        throw new MethodMismatchException(
          s"Conflicting REST method declared in the autodoc and in conf/routes for ${routeEntry.scalaClass}.${routeEntry.scalaMethod}.\n" +
          s"autodoc: $jsonMethod. conf/routes: ${routeEntry.restMethod}\n" +
          "(See README.md for more information)\n"
        )

      if (!hasSameUri(jsonUri, routeEntry.getDocUri)) {
        throw new UriMismatchException(
          s"Conflicting uri declared in the autodoc and in conf/routes for ${routeEntry.scalaClass}.${routeEntry.scalaMethod}.\n" +
          s"autodoc: $jsonUri. conf/routes: ${routeEntry.uri}\n" +
          "(See README.md for more information)\n"
        )
      }
    }
    )
  }

  /**
    * @return List of strings with api docs text. INCLUDE is expanded. The returned value is not a list of lines, but a list of multiline strings. Each multiline string contains the ApiDoc annotation of a method.
    */
  def getApiDocsFromAnnotations(routeEntries: List[RouteEntry] = routesHelper.getRouteEntries()): List[String] = {

    val routeEntriesWithoutApiDocs = routeEntries.filter(routeEntry => hasMethodAnnotation(routeEntry.scalaClass, routeEntry.scalaMethod))
    validate(routeEntriesWithoutApiDocs)

    val alreadyIncluded = scala.collection.mutable.Set[String]()

    val apiDocs = routeEntriesWithoutApiDocs.map(routeEntry =>
      getMethodAnnotationDoc(routeEntry.scalaClass, routeEntry.scalaMethod, alreadyIncluded)
    )

    apiDocs
  }

}
