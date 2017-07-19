package test

import org.specs2.mutable._
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test._

import javax.inject.Inject

import no.samordnaopptak.json._

import no.samordnaopptak.test.TestByAnnotation

import no.samordnaopptak.apidoc.{ApiDocValidation, ApiDocUtil, ApiDocParser, SwaggerUtil}


object SwaggerTestData{

  val apidocstrings = List(
    """
    GET /api/v1/users/{type}

    DESCRIPTION
      Get Users summary
      Get Users description

    ERRORS
      400 Something went wrong

    RESULT
      201: Array User <- An array of users
      405: Array String <- An array of strings

    PARAMETERS
      type: Enum(man, dog, woman) String
      age: Enum(2,65,9) Int(query, optional)

    User: !
      id: Long
      id2: Int(optional)
      names: Array String
      extra: Extra(optional)
      type: Enum(man, woman, dog) String(optional)
      age: Enum(2,65,9) String
      birthday: Date
      created: DateTime

    Extra: !
      extrastring: String <- Extra type
    """,



    """
    POST /api/v1/users/{type}

    DESCRIPTION
      Create User summary
      Create User description

    ERRORS
      400 Something went wrong

    RESULT
      201: User <- A User from post

    PARAMETERS
      body: User
      type: Enum(man, dog, woman) String

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
      202: Array String <- Cats
    """,

    """
    GET /api/v1/dogs

    DESCRIPTION
      Get Dogs
      Get Dogs description

    RESULT
      Enum(dog, cat, woman) String <- Type
    """

  )


    val jsonstring = s"""
{
  "swagger" : "2.0",
  "produces":["application/json"],

  "paths": {
    "/api/v1/users/{type}": {
        "get": {
              "summary":"Get Users summary",
              "description": "Get Users description",
              "parameters": [
                {
                  "in": "path",
                  "name": "type",
                  "description" : "",
                  "type": "string",
                  "enum": [
                     "man","dog","woman"
                  ],
                  "required" : true
                },
                {
                  "in": "query",
                  "name": "age",
                  "description" : "",
                  "type": "integer",
                  "format": "int32",
                  "enum": [
                     "2","65","9"
                  ],
                  "required" : false
                }
              ],
              "tags": [
                  "Users"
              ],
              "responses": {
                 "400" : {
                    "description" : "Something went wrong"
                 },
                 "201": {
                    "description": "An array of users",
                    "schema": {
                        "type": "array",
                        "items": {
                            "$$ref": "#/definitions/User"
                        }
                    }
                 },
                 "405": {
                    "description": "An array of strings",
                    "schema": {
                        "type": "array",
                        "items": {
                            "type": "string"
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
                      "$$ref": "#/definitions/User"
                  }
                },
                {
                  "in": "path",
                  "name": "type",
                  "required" : true,
                  "description" : "",
                  "type": "string",
                  "enum": [
                     "man","dog","woman"
                  ]
                }
              ],
              "tags": [
                  "Users"
              ],
              "responses": {
                 "400" : {
                    "description" : "Something went wrong"
                 },
                 "201": {
                    "description": "A User from post",
                    "schema": {
                       "$$ref": "#/definitions/User"
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
                  "type" : "string",
                  "in" : "path",
                  "required" : true
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
                 "202": {
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
    },

    "/api/v1/dogs": {
        "get" : {
              "summary": "Get Dogs",
              "description": "Get Dogs description",
              "parameters": [ ],
              "tags": [
                  "Dogs"
              ],
              "responses": {
                 "200": {
                    "description": "Type",
                    "schema": {
                        "type": "string",
                        "enum": [
                           "dog", "cat", "woman"
                        ]
                    }
                 }
              }
         }
    }
  },

  "definitions": {
        "User": {
            "required" : ["id", "names", "age", "birthday", "created", "${JsonMatcher.___ignoreOrder.value}"],
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
                    "$$ref": "#/definitions/Extra"
                },
                "type": {
                    "type": "string",
                    "enum": [
                       "man", "woman", "dog"
                    ]
                },
                "age": {
                    "type": "string",
                    "enum": [
                       "2","65","9"
                    ]
                },
                "birthday": {
                    "type": "string",
                    "format": "date"
                },
                "created": {
                    "type": "string",
                    "format": "date-time"
                }
            }
        },
        "Extra": {
            "required": ["extrastring"],
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

class SwaggerUtilSpec extends Specification with InjectHelper {

  lazy val apiDocValidation = inject[ApiDocValidation]
  lazy val apiDocUtil = inject[ApiDocUtil]

  val apiDocSamples = ApiDocSamples(apiDocValidation)

  "Swagger" should {

    "pass the annotation tests" in { // First run the smallest unit tests.
      play.api.test.Helpers.running(GuiceApplicationBuilder().build()) {
        TestByAnnotation.TestObject(SwaggerUtil)
      }
    }


    "Produce the main thing" in {
      val produced = apiDocUtil.getSwaggerDocs("/api/v1/", SwaggerTestData.apidocstrings)

      val correct = J.parse(SwaggerTestData.jsonstring)

      /*
      println("groups: "+SwaggerUtil.allTags("/api/v1/", SwaggerTestData.apidocstrings))
      //println("getjson: "+SwaggerUtil.getJson("/api/v1/", "/api/v1/users", no.samordnaopptak.apidoc.controllers.thething.apidocstrings))
      println()
       */

      JsonMatcher.matchJson(correct, produced)
    }

    "validate that all used datatype are defined" in {
      play.api.test.Helpers.running(FakeApplication()) {
        val apidocstrings = apiDocSamples.allAndMissingDataTypes
        val apiDocs = ApiDocParser.getApiDocs(apiDocValidation, apidocstrings)
        val dataTypes = ApiDocParser.getDataTypes(apiDocValidation, apidocstrings)

        SwaggerUtil.getMain("/api/v1/", apiDocs, dataTypes) should throwA(new Exception("""2 ApiDoc datatype(s) was/were undefined while evaluating "/api/v1/": ("StringSalabim", "StringSalami")"""))
      }
    }

  }
}
