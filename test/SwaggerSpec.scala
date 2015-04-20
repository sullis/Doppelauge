package test.lib

import org.specs2.mutable._
import play.api.test._
import play.api.libs.json._

import no.samordnaopptak.apidoc.TestByAnnotation
import no.samordnaopptak.apidoc.{SwaggerUtil}
import no.samordnaopptak.apidoc.JsonMatcher._

object SwaggerTestData{

  val apidocstrings = List(
    """
    GET /api/v1/users

    DESCRIPTION
      Get Users summary
      Get Users description

    ERRORS
      400 Something went wrong

    RESULT
      Array User <- An array of users

    User: !
      id: Long
      id2: Int
      names: Array String
      extra: Extra

    Extra: !
      extrastring: String <- Extra type
    """,



    """
    POST /api/v1/users

    DESCRIPTION
      Create User summary
      Create User description

    ERRORS
      400 Something went wrong

    RESULT
      User <- A User from post

    PARAMETERS
      body: User
    """,



    """
    GET /api/v1/users2/{id}

    DESCRIPTION
      Get User summary
      Get User description

    ERRORS
      400 Something went wrong

    RESULT
      String <- A User from get

    PARAMETERS
      id: String
    """,



    """
    GET /api/v1/cats

    DESCRIPTION
      Get Cats
      Get Cats description

    RESULT
      Array String <- Cats
    """

  )



  val jsonstring = """
{
  "swagger" : "2.0",
  "resourcePath":"/api/v1/",
  "produces":["application/json"],

  "paths": {
    "/api/v1/users": {
        "get": {
              "summary":"Get Users summary",
              "description": "Get Users description",
              "parameters": [],
              "tags": [
                  "Users"
              ],
              "responses": {
                 "400" : {
                    "description" : "Something went wrong"
                 },
                 "200": {
                    "description": "An array of users",
                    "schema": {
                        "type": "array",
                        "items": {
                            "$ref": "#/definitions/User"
                        }
                    }
                 }
              }
         },
        "post": {
              "summary":"Create User summary",
              "description": "Create User description",
              "parameters": [
                {
                  "in" : "body",
                  "name" : "body",
                  "required" : true,
                  "description" : "",
                  "schema": {
                      "$ref": "#/definitions/User"
                  }
                }
              ],
              "tags": [
                  "Users"
              ],
              "responses": {
                 "400" : {
                    "description" : "Something went wrong"
                 },
                 "200": {
                    "description": "A User from post",
                    "schema": {
                       "$ref": "#/definitions/User"
                    }
                 }
              }
         }
     },

    "/api/v1/users2/{id}": {
        "get" : {
              "summary":"Get User summary",
              "description": "Get User description",
              "parameters": [
                {
                  "name" : "id",
                  "required" : true,
                  "description" : "",
                  "schema" : {
                    "type" : "string"
                  },
                  "in" : "path"
                }
              ],
              "tags": [
                  "Users2"
              ],
              "responses": {
                 "400" : {
                    "description" : "Something went wrong"
                 },
                 "200": {
                     "description": "A User from get",
                     "schema": {
                       "type": "string"
                     }
                 }
              }
         }
    },

    "/api/v1/cats": {
        "get" : {
              "summary": "Get Cats",
              "description": "Get Cats description",
              "parameters": [ ],
              "tags": [
                  "Cats"
              ],
              "responses": {
                 "200": {
                    "description": "Cats",
                    "schema": {
                        "type": "array",
                        "items": {
                            "type": "string"
                        }
                    }
                 }
              }
         }
    }
  },

  "definitions": {
        "User": {
            "properties": {
                "id": {
                    "type": "integer",
                    "format": "int64"
                }, 
                "id2": {
                    "type": "integer",
                    "format": "int32"
                },
               "names": {
                    "type": "array",
                    "items": {
                       "type": "string"
                    }
                },
                "extra": {
                    "$ref": "#/definitions/Extra"
                }
            }
        },
        "Extra": {
            "properties": {
               "extrastring": {
                  "type": "string",
                  "description": "Extra type"
               }
            }
        }
  }
}
"""

}


class SwaggerSpec extends Specification {

  "Swagger" should {

    /*
    "pass the annotation tests" in { // First run the smallest unit tests.
      play.api.test.Helpers.running(FakeApplication()) {
        TestByAnnotation.TestObject(SwaggerUtil)
      }
      true
    }
     */

    "something" in {
      //val a = no.samordnaopptak.apidoc.ApiDocUtil.getRaw(ApiDocSamples.docWithMissingDataTypes)
      val a = no.samordnaopptak.apidoc.ApiDocUtil.getJson(ApiDocSamples.docWithArrayResult)
      println("a: "+Json.prettyPrint(a))

      matchJson(
        a,
        Json.obj(
          "method" -> "GET",
          "uri"    -> "/api/v1/usernames",
          "uriParms" -> Json.arr(),
          "shortDescription" -> "Get Usernames",
          "longDescription" -> "",
          "result" -> Json.obj(
            "type" -> "String",
            "comment" -> "",
            "isArray" -> true
          )
        )
      )

      true
    }

    "Produce the main thing" in {
      val produced = SwaggerUtil.getMain("/api/v1/", SwaggerTestData.apidocstrings)
      val correct = Json.parse(SwaggerTestData.jsonstring)

      /*
      println("groups: "+SwaggerUtil.allTags("/api/v1/", SwaggerTestData.apidocstrings))
      //println("getjson: "+SwaggerUtil.getJson("/api/v1/", "/api/v1/users", no.samordnaopptak.apidoc.controllers.thething.apidocstrings))
      println()
       */

      matchJson(produced, correct)
    }

    "validate that a datatype is only defined one place" in {
      play.api.test.Helpers.running(FakeApplication()) {
        SwaggerUtil.getMain("/api/v1/", ApiDocSamples.allAndExtraDataType) should throwA(new Exception("One or more ApiDoc datatypes defined more than once: List(Attributes)"))
      }
    }

    "validate that all used datatype are defined" in {
      play.api.test.Helpers.running(FakeApplication()) {
        SwaggerUtil.getMain("/api/v1/", ApiDocSamples.allAndMissingDataTypes) should throwA(new Exception("""2 ApiDoc datatype(s) was/were undefined while evaluating "/api/v1/": ("StringSalabim", "StringSalami")"""))
      }
    }

  }
}
