package test

import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._

import no.samordnaopptak.apidoc.{AnnotationHelper}
import no.samordnaopptak.apidoc.{ApiDoc, SwaggerUtil, RoutesHelper, RouteEntry}
import no.samordnaopptak.test.TestByAnnotation


class Include {
  @ApiDoc(doc="""
    PARAMETERS 
      id: String (header) <- ID of the user

    INCLUDE test.Include.includedDocFunc
    INCLUDE test.Include.includedDocFunc
  """)
  def includedDocFunc() =
    throw new Exception("No point calling this function")
}

object AnnotationHelperData{
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

    INCLUDE test.Include.includedDocFunc
    INCLUDE test.Include.includedDocFunc

    ERRORS
      400 User not found
      400 Syntax Error

    RESULT
      User
  """)
  def errorDoc2() = true

  def errorDoc3() = true // testing missing api-doc validattion
}


class AnnotationHelperSpec extends Specification {

  import AnnotationHelperData._

  def inCleanEnvironment()(func: => Unit): Boolean = {
    running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      func
    }
    true
  }

  def routeEntries =
    RoutesHelper.getRouteEntries()
      .filter(_.scalaClass != "controllers.Assets") // no api-doc for the static assets files


  "AnnotationHelper" should {

    "pass the annotation tests" in { // First run the smallest unit tests.
      TestByAnnotation.TestObject(AnnotationHelper)
      true
    }

    "validate that the validate method that validates if method and uri in conf/routes and autodoc matches works" in {
      inCleanEnvironment() {

        AnnotationHelper.validate(routeEntries)

        {
          val validRouteEntry = RouteEntry("GET", "/api/v1/users/$id<[^/]+>", "test.AnnotationHelperData", "errorDoc")
          AnnotationHelper.validate(validRouteEntry::routeEntries)
        }

        {
          val errorRouteEntry = RouteEntry("GET", "/api/v1/flapp", "test.AnnotationHelperData", "errorDoc")
          AnnotationHelper.validate(errorRouteEntry::routeEntries) should throwA[AnnotationHelper.UriMismatchException]
        }

        {
          val errorRouteEntry = RouteEntry("PUT", "/api/v1/users/$id<[^/]+>", "test.AnnotationHelperData", "errorDoc")
          AnnotationHelper.validate(errorRouteEntry::routeEntries) should throwA[AnnotationHelper.MethodMismatchException]
        }

        {
          val errorRouteEntry = RouteEntry("GET", "/api/v1/users", "test.AnnotationHelperData", "errorDoc")
          AnnotationHelper.validate(errorRouteEntry::routeEntries) should throwA[AnnotationHelper.UriMismatchException]
        }

        {
          val errorRouteEntry = RouteEntry("GET", "/api/v1/users/$id<[^/]+>", "test.AnnotationHelperData", "errorDoc2")
          AnnotationHelper.validate(errorRouteEntry::routeEntries) should throwA[AnnotationHelper.UriMismatchException]
        }

        // The function  misses error doc
        {
          val errorRouteEntry = RouteEntry("GET", "/api/v1/somewhere", "test.AnnotationHelperData", "errorDoc3")
          AnnotationHelper.validate(errorRouteEntry::routeEntries) should throwA[AnnotationHelper.MissingMethodException]
        }

        // The function itself is missing
        {
          val errorRouteEntry = RouteEntry("GET", "/api/v1/somewhere", "test.AnnotationHelperData", "errorDoc3NotHere")
          AnnotationHelper.validate(errorRouteEntry::routeEntries) should throwA[AnnotationHelper.MissingMethodException]
        }

      }
    }

    "validate all route entries" in {
      inCleanEnvironment() {
        AnnotationHelper.validate(routeEntries)
      }
    }


  }

}

