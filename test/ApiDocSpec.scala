package test.lib

import org.specs2.mutable._
import play.api.test._
import play.api.libs.json._

import no.samordnaopptak.apidoc.{ApiDocUtil, JsonMatcher}

import com.fasterxml.jackson.annotation._

case class User(id: String, @JsonIgnore ignored_parameter: Int = 1){
  val attributes: Set[String] = Set()
  val unrelated: String = "a"
  @JsonIgnore val ignored_field: Int = 2
}


object ApiDocSamples{
  val doc1 = """
    GET /api/v1/users/{id}

    DESCRIPTION
      Get all users
      More detailed description of "Get all users"

    PARAMETERS
      id: String <- ID of the user
      id2: String (header)
      q: String (query)
      body: User 

    ERRORS
      400 What?
      401 Syntax Error

    RESULT
      User <- Result comment

    User:          test.lib.User             (+something,   -unrelated)
      id: String <- The ID of the user
      attributes: Array Attributes
      something: String

    Attributes: !
      firstAndLast: Array String
      ...
  """

  val doc2 = """
    GET /api/v1/users

    DESCRIPTION
      Get all users

    RESULT
      String
  """

  val doc3 = """
    PUT /api/v1/users/{id}

    DESCRIPTION
      Put a user

    PARAMETERS
      id: String <- ID of the user

    RESULT
      String
  """

  val doc3b = """
    PUT /api/v1/users/{id}/hepp/{id2}

    DESCRIPTION
      Put a hepp user

    PARAMETERS
      id: String <- ID of the user
      id2: String <- ID2 of the user

    RESULT
      String
  """

  val doc4 = """
    GET /api/v1/acl

    DESCRIPTION
      Get all acls

    RESULT
      String
  """

  val doc5 = """
    GET /api/v1/acl/dasacls

    DESCRIPTION
      Get all das acls

    RESULT
      String
  """

  val docWithExtraDataType = """
    GET /api/v1/acl

    DESCRIPTION
      Get all acls

    RESULT
      String

    Attributes: !
      firstAndLast: Array String
      hp: Ag
  """

  val docWithMissingDataTypes = """
    GET /api/v1/acl

    DESCRIPTION
      Get all acls

    PARAMETERS
      id: StringSalabim (body)

    RESULT
      StringSalami
  """

  val docWithIllegalParamType = """
    GET /api/v1/acl

    DESCRIPTION
      Get all acls

    PARAMETERS
      id: String (gakk)
  """

  val docWithUndefinedClass = """
    GET /api/v1/acl

    DESCRIPTION
      Get all acls

    UnknownType:
      id: String
  """

  val docWithMismatchedPathParameters1 = """
    GET /api/v1/acl/{id}

    DESCRIPTION
      Get all acls
  """

  val docWithMismatchedPathParameters2 = """
    GET /api/v1/acl/

    DESCRIPTION
      Get all acls

    PARAMETERS
      id: String
  """

  val docWithMismatchedPathParameters3 = """
    GET /api/v1/acl/{id}

    DESCRIPTION
      Get all acls

    PARAMETERS
      id2: String
  """

  val docWithMismatchedPathParameters4 = """
    GET /api/v1/acl/{id}

    DESCRIPTION
      Get all acls

    PARAMETERS
      id:  String
      id2: String
  """

  val docWithMismatchedClass = """
    GET /api/v1/acl

    DESCRIPTION
      Get all acls

    UnknownType: test.lib.User
      id: String
  """

  val docWithClassFromModels = """
    GET /api/v1/acl

    DESCRIPTION
      Get all acls

    UnknownType: InstitutionRole(-ids,-jsValue)
      id_institution: String
      id_role: String
      institution: String
      role: String
  """

  val docWithClassWithSameName1 = """
    GET /api/v1/acl

    DESCRIPTION
      Get all acls

    test.lib.User:
      id: String
      attributes: Array String
      unrelated: String
  """

  val docWithClassWithSameName2 = """
    GET /api/v1/acl

    DESCRIPTION
      Get all acls

    test.lib.User(-id, +id2):
      id2: String
      attributes: Array String
      unrelated: String
  """

  val allUsers = List(doc1,doc2,doc3,doc3b)
  val allAcls = List(doc4,doc5)
  val all = allUsers ++ allAcls

  val allAndExtraDataType = all ++ List(docWithExtraDataType)
  val allAndMissingDataTypes = all ++ List(docWithMissingDataTypes)
}


class ApiDocSpec extends Specification {

  class Inner1{
    case class Inner2()
  }

  "ApiDoc" should {

    "Get Class objects from inner classes" in {
      play.api.test.Helpers.running(FakeApplication()) {
        ApiDocUtil.loadInnerClass("test.lib.ApiDocSpec.Inner1.Inner2")
        true
      }
    }

    "Check that the findUriParm function works" in {
      ApiDocUtil.findUriParm("/api/v1/acl/{service}") must equalTo(List("service"))
      ApiDocUtil.findUriParm("/api/v1/acl/{service}/{hest}") must equalTo(List("service", "hest"))
      ApiDocUtil.findUriParm("/api/v1/acl/")          must equalTo(List())
      ApiDocUtil.findUriParm("/api/v1/acl")           must equalTo(List())
    }

    "Validate data type fields, with no added or removed fields" in {
      play.api.test.Helpers.running(FakeApplication()) {
        ApiDocUtil.validateDataTypeFields("test.lib.User", "hepp", Set("id", "attributes", "unrelated"),        Set(), Set())
        ApiDocUtil.validateDataTypeFields("test.lib.User", "hepp", Set("id"),                                   Set(), Set()) should throwA[ApiDocUtil.MismatchFieldException]
        ApiDocUtil.validateDataTypeFields("test.lib.User", "hepp", Set("id", "attributes", "unrelated", "id2"), Set(), Set()) should throwA[ApiDocUtil.MismatchFieldException]
      }
    }

    "Validate data type fields, with added field" in {
      play.api.test.Helpers.running(FakeApplication()) {
        ApiDocUtil.validateDataTypeFields("test.lib.User", "hepp", Set("id", "id2", "attributes", "unrelated"),        Set("id2"), Set())
        ApiDocUtil.validateDataTypeFields("test.lib.User", "hepp", Set("id", "id2", "attributes", "unrelated"),        Set("id"),  Set()) should throwA[ApiDocUtil.AlreadyDefinedFieldException]
        ApiDocUtil.validateDataTypeFields("test.lib.User", "hepp", Set("id", "attributes", "unrelated"),               Set("id2"), Set()) should throwA[ApiDocUtil.MismatchFieldException]
        ApiDocUtil.validateDataTypeFields("test.lib.User", "hepp", Set("id", "id2", "id3", "attributes", "unrelated"), Set("id2"), Set()) should throwA[ApiDocUtil.MismatchFieldException]
      }
    }

    "Validate data type fields, with removed field" in {
      play.api.test.Helpers.running(FakeApplication()) {
        ApiDocUtil.validateDataTypeFields("test.lib.User", "hepp", Set("attributes", "unrelated"),              Set(),     Set("id"))
        ApiDocUtil.validateDataTypeFields("test.lib.User", "hepp", Set("id","attributes", "unrelated"),         Set("id"), Set("id"))  should throwA[ApiDocUtil.AlreadyDefinedFieldException]
        ApiDocUtil.validateDataTypeFields("test.lib.User", "hepp", Set("unrelated"),                            Set(),     Set("id"))  should throwA[ApiDocUtil.MismatchFieldException]
        ApiDocUtil.validateDataTypeFields("test.lib.User", "hepp", Set("id", "attributes", "unrelated"),        Set(),     Set("id2")) should throwA[ApiDocUtil.UnknownFieldException]
      }
    }

    "Throw exception for badly formatted indentation (the first line sets the indentation standard, which the rest of the lines must follow)" in {
      ApiDocUtil.getRaw("""
         HALLO
       HALLO2
      """) must throwA(new Exception("""Bad indentation for line "HALLO2""""))
    }

    "Throw exception for illegal parameter type" in {
      ApiDocUtil.getJson(ApiDocSamples.docWithIllegalParamType) must throwA(
        new Exception(""""gakk" is not a valid paramameter type. It must be either "body", "path", "query", "header", or "form". See https://github.com/wordnik/swagger-core/wiki/Parameters""")
      )
    }

    "Get ClassNotFoundException for datatype with undefined corresponding scala class" in {
      play.api.test.Helpers.running(FakeApplication()) {
        ApiDocUtil.getJson(ApiDocSamples.docWithUndefinedClass) must throwA[java.lang.ClassNotFoundException]
      }
    }

    "Test getting class from models instead, if we get ClassNotFoundException" in {
      play.api.test.Helpers.running(FakeApplication()) {
        if (play.api.Play.current.configuration.getString("application.name").get == "studieadmin")
          ApiDocUtil.getJson(ApiDocSamples.docWithClassFromModels)
      }
      true
    }

    "Get ApiDocUtil.MismatchFieldException for datatype with mismatched corresponding scala class" in {
      play.api.test.Helpers.running(FakeApplication()) {
        ApiDocUtil.getJson(ApiDocSamples.docWithMismatchedClass) must throwA[ApiDocUtil.MismatchFieldException]
      }
    }

    "Get ApiDocUtil.MismatchPathParametersException for datatype with mismatched path parameters" in {
      play.api.test.Helpers.running(FakeApplication()) {
        ApiDocUtil.getJson(ApiDocSamples.docWithMismatchedPathParameters1) must throwA[ApiDocUtil.MismatchPathParametersException]
        ApiDocUtil.getJson(ApiDocSamples.docWithMismatchedPathParameters2) must throwA[ApiDocUtil.MismatchPathParametersException]
        ApiDocUtil.getJson(ApiDocSamples.docWithMismatchedPathParameters3) must throwA[ApiDocUtil.MismatchPathParametersException]
        ApiDocUtil.getJson(ApiDocSamples.docWithMismatchedPathParameters4) must throwA[ApiDocUtil.MismatchPathParametersException]
      }
    }

    "Use class name with same name as datatype, if class name for datatype is undefined" in {
      play.api.test.Helpers.running(FakeApplication()) {
        ApiDocUtil.getJson(ApiDocSamples.docWithClassWithSameName1)
        ApiDocUtil.getJson(ApiDocSamples.docWithClassWithSameName2)
      }
      true
    }

    "Create raw Json data from ApiDoc" in {
      JsonMatcher.matchJson(
        Json.obj(
          "GET /api/v1/users/{id}" -> Json.arr(),
          "DESCRIPTION" -> Json.arr(
            "Get all users",
            """More detailed description of "Get all users""""),
          "PARAMETERS" -> Json.arr(
            "id: String <- ID of the user",
            "id2: String (header)",
            "q: String (query)",
            "body: User"),
          "ERRORS" -> Json.arr(
            "400 What?",
            "401 Syntax Error"),
          "RESULT" -> Json.arr(
            "User <- Result comment"),
          "User:          test.lib.User             (+something,   -unrelated)" -> Json.arr(
            "id: String <- The ID of the user",
            "attributes: Array Attributes",
            "something: String"
          ),
          "Attributes: !" -> Json.arr(
            "firstAndLast: Array String",
            "..."
          )
        ),
        ApiDocUtil.getRaw(ApiDocSamples.doc1)
      )
    }

    "Create Json data from ApiDoc" in {
      JsonMatcher.matchJson(
        Json.obj(
          "method" -> "GET",
          "uri"    -> "/api/v1/users/{id}",
          "uriParms" -> Json.arr("id"),
          "shortDescription" -> "Get all users",
          "longDescription" -> """More detailed description of "Get all users"""",
          "parameters" -> Json.obj(
            "id" -> Json.obj(
              "type" -> "String",
              "comment" -> "ID of the user",
              "isArray" -> false,
              "paramType" -> "path"
            ),
            "id2" -> Json.obj(
              "type" -> "String",
              "noComment" -> true,
              "isArray" -> false,
              "paramType" -> "header"
            ),
            "q" -> Json.obj(
              "type" -> "String",
              "noComment" -> true,
              "isArray" -> false,
              "paramType" -> "query"
            ),
            "body" -> Json.obj(
              "type" -> "User",
              "noComment" -> true,
              "isArray" -> false,
              "paramType" -> "body"
            )),
          "errors" -> Json.arr(
            Json.obj(
              "code" -> 400,
              "message" -> "What?"),
            Json.obj(
              "code" -> 401,
              "message" -> "Syntax Error")
          ),
          "result" -> Json.obj(
            "type" -> "User",
            "comment" -> "Result comment"
          )
        ),
        play.api.test.Helpers.running(FakeApplication()) {
          ApiDocUtil.getJson(ApiDocSamples.doc1)
        }
      )
    }

    "Get datatypes from ApiDoc" in {
      JsonMatcher.matchJson(
        Json.obj(
          "User" -> Json.obj(
            "id" -> Json.obj(
              "type" -> "String",
              "comment" -> "The ID of the user",
              "isArray" -> false,
              "paramType" -> "path"
            ),
            "attributes" -> Json.obj(
              "type" -> "Attributes",
              "noComment" -> true,
              "isArray" -> true,
              "paramType" -> "path"
            ),
            "something" -> Json.obj(
              "type" -> "String",
              "noComment" -> true,
              "isArray" -> false,
              "paramType" -> "path"
            )
          ),
          "Attributes" -> Json.obj(
            "firstAndLast" -> Json.obj(
              "type" -> "String",
              "noComment" -> true,
              "isArray" -> true,
              "paramType" -> "path"
            ),
            "..." -> Json.obj(
              "type" -> "etc.",
              "isArray" -> false
            )
          )
        ),
        play.api.test.Helpers.running(FakeApplication()) {
          ApiDocUtil.getDataTypes(ApiDocSamples.doc1)
        }
      )
    }
  }
}


