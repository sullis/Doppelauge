package test.lib

import org.specs2.mutable._
import play.api.test._
import play.api.libs.json._

import no.samordnaopptak.apidoc.{SwaggerUtil}
import no.samordnaopptak.apidoc.JsonMatcher._

class SwaggerSpec extends Specification {

  "Swagger" should {

    "Filter out correct resource path from uri" in {
      SwaggerUtil.getResourcePath("/api/v1/users/") must equalTo("/users")
      SwaggerUtil.getResourcePath("/api/v1/users/{id}") must equalTo("/users")
      SwaggerUtil.getResourcePath("/api/v1/users") must equalTo("/users")
      SwaggerUtil.getResourcePath("/users") must equalTo("/users")
    }

    "Filter out correct resource path group from basepath and uri" in {
      SwaggerUtil.getResourcePathGroup("/api/v1/", "/api/v1/users/habla/happ") must equalTo("users")
      SwaggerUtil.getResourcePathGroup("/api/v1/", "/api/v1/users/habla/") must equalTo("users")
      SwaggerUtil.getResourcePathGroup("/api/v1/", "/api/v1/users/habla/{id}") must equalTo("users")
      SwaggerUtil.getResourcePathGroup("/api/v1/", "/api/v1/users/habla") must equalTo("users")
      SwaggerUtil.getResourcePathGroup("/api/v1/", "/api/v1/users/{id}") must equalTo("users")
      SwaggerUtil.getResourcePathGroup("/api/v1/", "/api/v1/users/") must equalTo("users")
      SwaggerUtil.getResourcePathGroup("/api/v1/", "/api/v1/users") must equalTo("users")
    }

    "Get all resource paths" in {
      play.api.test.Helpers.running(FakeApplication()) {
        SwaggerUtil.allResourcePaths(ApiDocSamples.allUsers) must equalTo(Set("/users"))
        SwaggerUtil.allResourcePaths(ApiDocSamples.allAcls) must equalTo(Set("/acl", "/dasacls"))
        SwaggerUtil.allResourcePaths(ApiDocSamples.all) must equalTo(Set("/acl", "/dasacls", "/users"))
      }
    }

    "Get all resource path groups for /api/v1/" in {
      play.api.test.Helpers.running(FakeApplication()) {
        val basePath = "/api/v1/"
        SwaggerUtil.allResourcePathGroups(basePath,ApiDocSamples.allUsers) must equalTo(Set("users"))
        SwaggerUtil.allResourcePathGroups(basePath,ApiDocSamples.allAcls) must equalTo(Set("acl"))
        SwaggerUtil.allResourcePathGroups(basePath,ApiDocSamples.all) must equalTo(Set("acl", "users"))
      }
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
              "path" -> "/../../../api/v1/users/{id}")
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
       "nickname" : "",
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
         "required" : true
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
