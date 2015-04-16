package test.lib

import org.specs2.mutable._
import play.api.test._
import play.api.libs.json._

import no.samordnaopptak.apidoc.TestByAnnotation
import no.samordnaopptak.apidoc.{SwaggerUtil}
import no.samordnaopptak.apidoc.JsonMatcher._


class SwaggerSpec extends Specification {

  "Swagger" should {

    "pass the annotation tests" in { // First run the smallest unit tests.
      play.api.test.Helpers.running(FakeApplication()) {
        TestByAnnotation.TestObject(SwaggerUtil)
      }
      true
    }


    "Get all /users Apis" in {
      matchJson(
        Json.obj(
          "models" -> Json.obj(
            "Attributes" -> Json.obj(___numElements -> 2),
            "User" -> Json.obj(___numElements -> 2)),
          "resourcePath" -> "/users",
          "apis" -> Json.arr(
            Json.obj(
              "operations" -> Json.arr(Json.obj(
                "method" -> "GET",
                ___numElements -> 7)),
              "path" -> "/../../../api/v1/users/{id}"),
            Json.obj(
              "operations" -> Json.arr(Json.obj(
                "method" -> "GET",
                ___numElements -> 7)),
              "path" -> "/../../../api/v1/users"),
            Json.obj(
              "operations" -> Json.arr(Json.obj(
                "method" -> "PUT",
                ___numElements -> 7)),
              "path" -> "/../../../api/v1/users/{id}"),
            Json.obj(
              "operations" -> Json.arr(Json.obj(
                "method" -> "PUT",
                ___numElements -> 7)),
              "path" -> "/../../../api/v1/users/{id}/hepp/{id2}"),
            Json.obj(
              "operations" -> Json.arr(Json.obj(
                "method" -> "PATCH",
                ___numElements -> 7)),
              "path" -> "/../../../api/v1/users/{id}"),
            Json.obj(
              "operations" -> Json.arr(Json.obj(
                "method" -> "PATCH",
                ___numElements -> 7)),
              "path" -> "/../../../api/v1/users/{id}/hepp/{id2}")
          ),
          ___allowOtherFields
        ),
        play.api.test.Helpers.running(FakeApplication()) {
          SwaggerUtil.getJson("/api/v1/", ApiDocSamples.all, "users")
        }
      )
    }

    "Get all /acl Apis" in {
      matchJson(
        Json.obj(
          "models" -> Json.obj(
            "Attributes" -> Json.obj(___numElements -> 2),
            "User" -> Json.obj(___numElements -> 2)),
          "resourcePath" -> "/acl",
          "apis" -> Json.arr(
            Json.obj(
              "operations" -> Json.arr(Json.obj(
                "method" -> "GET",
                ___numElements -> 7)),
              "path" -> "/../../../api/v1/acl"),
            Json.obj(
              "operations" -> Json.arr(Json.obj(
                "method" -> "GET",
                ___numElements -> 7)),
              "path" -> "/../../../api/v1/acl/dasacls")
          ),
          ___allowOtherFields
        ),
        play.api.test.Helpers.running(FakeApplication()) {
          SwaggerUtil.getJson("/api/v1/", ApiDocSamples.all, "acl")
        }
      )
    }


    "Get main json object (The Swagger menu)" in {
      matchJson(
        Json.obj(
          "info" -> Json.obj(___allowOtherFields),
          "apis" -> Json.arr(
            Json.obj(
              "path" -> "/acl",
              "description" -> "Operations on acl"),
            Json.obj(
              "path" -> "/users",
              "description" -> "Operations on users")
          ),
          ___allowOtherFields
        ),
        play.api.test.Helpers.running(FakeApplication()) {
          SwaggerUtil.getMain("/api/v1/", ApiDocSamples.all)
        }
      )
    }

    "validate that a datatype is only defined one place" in {
      play.api.test.Helpers.running(FakeApplication()) {
        SwaggerUtil.getJson("/api/v1/", ApiDocSamples.allAndExtraDataType, "users") should throwA(new Exception("One or more ApiDoc datatypes defined more than once: List(Attributes)"))
      }
    }

    "validate that all used datatype are defined" in {
      play.api.test.Helpers.running(FakeApplication()) {
        SwaggerUtil.getJson("/api/v1/", ApiDocSamples.allAndMissingDataTypes, "acl") should throwA(new Exception("""2 ApiDoc datatype(s) was/were undefined while evaluating "acl": ("StringSalabim", "StringSalami")"""))
      }
    }

    "Create Swagger apidoc from Apidoc apidoc (check all fields)" in {
      matchJson(
        Json.parse(
"""
{
  "apiVersion":"1",
  "swaggerVersion":"1.2",
  "basePath":"",
  "resourcePath" : "/users",

  "produces":["application/json"],

   "apis" : [ {
     "path" : "/../../../api/v1/users/{id}",
     "operations" : [ {
       "method" : "GET",
       "summary" : "Get all users",
       "notes" : "More detailed description of \"Get all users\"",
       "type" : "User",
       "nickname" : "habla",
       "parameters" : [ {
         "name" : "id",
         "required" : true,
         "type" : "String",
         "description" : "ID of the user",
         "paramType" : "path"
       }, {
         "name" : "id2",
         "paramType" : "header",
         "type" : "String",
         "required" : true
       }, {
         "name" : "q",
         "paramType" : "query",
         "type" : "String",
         "required" : false
       }, {
         "name" : "body",
         "required" : true,
         "type" : "User",
         "paramType" : "body"
       } ],
       "responseMessages" : [ {
         "code" : 400,
         "message" : "What?"
       }, {
         "code" : 401,
         "message" : "Syntax Error"
       } ]
     } ]
   } ],

  "models":{
   "User" : {
     "id" : "User",
     "properties" : {
       "id" : {
         "type" : "String",
         "description" : "The ID of the user",
         "required" : true
       },
       "attributes" : {
         "type" : "array",
         "items" : {
           "$ref" : "Attributes"
         },
         "required" : true
       },
       "something" : {
         "type" : "String",
         "required" : true
       }
     }
   },
   "Attributes" : {
     "id" : "Attributes",
     "properties" : {
       "firstAndLast" : {
         "type" : "array",
         "items" : {
           "$ref" : "String"
         },
         "required" : true
       },
       "..." : {
         "type" : "etc.",
         "required" : true
       }
     }
   }
 }
}
"""
        ),
        play.api.test.Helpers.running(FakeApplication()) {
          SwaggerUtil.getJson("/api/v1/", ApiDocSamples.doc1)
        }
      )
    }

  }
}
