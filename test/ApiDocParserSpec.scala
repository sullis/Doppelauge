package test

import org.specs2.mutable._
import play.api.test._

import no.samordnaopptak.json._

import no.samordnaopptak.test.TestByAnnotation

import no.samordnaopptak.apidoc.{ApiDocParser, ApiDocValidation}

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
      q: String (query, optional)
      body: User 

    ERRORS
      400 What?
      401 Syntax Error

    RESULT
      203: User <- Result comment
      405: String <- Error comment

    User:          test.User             (+something,   -unrelated)
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
      201: String
  """

  val doc3 = """
    PUT /api/v1/users/{id}

    DESCRIPTION
      Put a user

    PARAMETERS
      id: String <- ID of the user

    RESULT
      202: String <- Hello
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
      Get all acls, several return types

    RESULT
      203: String <- result1
      204: Int <- result2
      404: Array String <- Not found
  """

  val doc5 = """
    GET /api/v1/acl/dasacls

    DESCRIPTION
      Get all das acls

    RESULT
      String
  """

  val doc6 = """
    PATCH /api/v1/users/{id}

    DESCRIPTION
      Patch a user

    PARAMETERS
      id: String <- ID of the user

    RESULT
      204: String
  """

  val doc6b = """
    PATCH /api/v1/users/{id}/hepp/{id2}

    DESCRIPTION
      Patch a hepp user

    PARAMETERS
      id: String <- ID of the user
      id2: String <- ID2 of the user

    RESULT
      String
  """

  val docWithArrayResult = """
    GET /api/v1/usernames

    DESCRIPTION
      Get Usernames

    RESULT
      205: Array String
  """

  val docWithEnums = """
    GET /api/v1/username/{id}

    DESCRIPTION
      Get Username

    PARAMETERS
      id: Enum(1,2,3) String
      body: EnumModel

    RESULT
      Enum(a,b,c,4) String

    EnumModel: !
      enumVal: Enum(a1,b2,c3) String
  """

  val docWithExtraDataType = """
    GET /api/v1/acl

    DESCRIPTION
      Get all acls

    RESULT
      206: String

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

    UnknownType: test.User
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

    test.User:
      id: String
      attributes: Array String
      unrelated: String
  """

  val docWithClassWithSameName2 = """
    GET /api/v1/acl

    DESCRIPTION
      Get all acls

    test.User(-id, +id2):
      id2: String
      attributes: Array String
      unrelated: String
  """

  val allUsers = List(doc1,doc2,doc3,doc3b,doc6,doc6b,docWithArrayResult)
  val allAcls = List(doc4,doc5)
  val all = allUsers ++ allAcls

  val allAndExtraDataType = all ++ List(docWithExtraDataType)
  val allAndMissingDataTypes = all ++ List(docWithMissingDataTypes)
}


class ApiDocParserSpec extends Specification {

  "ApiDocParser" should {

    "pass the annotation tests" in { // First run the smallest unit tests.
      play.api.test.Helpers.running(FakeApplication()) {
        TestByAnnotation.TestObject(ApiDocParser)
      }
      true
    }


    "Throw exception for badly formatted indentation (the first line sets the indentation standard, which the rest of the lines must follow)" in {
      ApiDocParser.getRaw("""
         HALLO
       HALLO2
      """) must throwA(new Exception("""Bad indentation for line "HALLO2""""))
    }

    "Throw exception for illegal parameter type" in {
      ApiDocParser.getJson(ApiDocSamples.docWithIllegalParamType) must throwA(
        new Exception(""""gakk" is not a valid paramameter type. It must be either "body", "path", "query", "header", or "formData". See https://github.com/wordnik/swagger-core/wiki/Parameters""")
      )
    }

    "Get ClassNotFoundException for datatype with undefined corresponding scala class" in {
      play.api.test.Helpers.running(FakeApplication()) {
        ApiDocParser.getJson(ApiDocSamples.docWithUndefinedClass) must throwA[java.lang.ClassNotFoundException]
      }
    }

    "Test getting class from models instead, if we get ClassNotFoundException" in {
      play.api.test.Helpers.running(FakeApplication()) {
        if (play.api.Play.current.configuration.getString("application.name").get == "studieadmin")
          ApiDocParser.getJson(ApiDocSamples.docWithClassFromModels)
      }
      true
    }

    "Get ApiDocParser.MismatchFieldException for datatype with mismatched corresponding scala class" in {
      play.api.test.Helpers.running(FakeApplication()) {
        ApiDocParser.getJson(ApiDocSamples.docWithMismatchedClass) must throwA[ApiDocValidation.MismatchFieldException]
      }
    }

    "Get ApiDocParser.MismatchPathParametersException for datatype with mismatched path parameters" in {
      play.api.test.Helpers.running(FakeApplication()) {
        ApiDocParser.getJson(ApiDocSamples.docWithMismatchedPathParameters1) must throwA[ApiDocValidation.MismatchPathParametersException]
        ApiDocParser.getJson(ApiDocSamples.docWithMismatchedPathParameters2) must throwA[ApiDocValidation.MismatchPathParametersException]
        ApiDocParser.getJson(ApiDocSamples.docWithMismatchedPathParameters3) must throwA[ApiDocValidation.MismatchPathParametersException]
        ApiDocParser.getJson(ApiDocSamples.docWithMismatchedPathParameters4) must throwA[ApiDocValidation.MismatchPathParametersException]
      }
    }

    "Use class name with same name as datatype, if class name for datatype is undefined" in {
      play.api.test.Helpers.running(FakeApplication()) {
        ApiDocParser.getJson(ApiDocSamples.docWithClassWithSameName1)
        ApiDocParser.getJson(ApiDocSamples.docWithClassWithSameName2)
      }
      true
    }

    "Create raw Json data from ApiDoc" in {
      JsonMatcher.matchJson(
        J.obj(
          "GET /api/v1/users/{id}" -> J.arr(),
          "DESCRIPTION" -> J.arr(
            "Get all users",
            """More detailed description of "Get all users""""),
          "PARAMETERS" -> J.arr(
            "id: String <- ID of the user",
            "id2: String (header)",
            "q: String (query, optional)",
            "body: User"),
          "ERRORS" -> J.arr(
            "400 What?",
            "401 Syntax Error"),
          "RESULT" -> J.arr(
            "203: User <- Result comment",
            "405: String <- Error comment"
          ),
          "User:          test.User             (+something,   -unrelated)" -> J.arr(
            "id: String <- The ID of the user",
            "attributes: Array Attributes",
            "something: String"
          ),
          "Attributes: !" -> J.arr(
            "firstAndLast: Array String",
            "..."
          )
        ),
        ApiDocParser.getRaw(ApiDocSamples.doc1)
      )
    }

    "Create Json data from ApiDoc" in {
      JsonMatcher.matchJson(
        J.obj(
          "method" -> "GET",
          "uri"    -> "/api/v1/users/{id}",
          "uriParms" -> J.arr("id"),
          "shortDescription" -> "Get all users",
          "longDescription" -> """More detailed description of "Get all users"""",
          "parameters" -> J.obj(
            "id" -> J.obj(
              "type" -> "String",
              "comment" -> "ID of the user",
              "isArray" -> false,
              "isEnum" -> false,
              "enumArgs" -> J.arr(),
              "paramType" -> "path",
              "required" -> true
            ),
            "id2" -> J.obj(
              "type" -> "String",
              "noComment" -> true,
              "isArray" -> false,
              "isEnum" -> false,
              "enumArgs" -> J.arr(),
              "paramType" -> "header",
              "required" -> true
            ),
            "q" -> J.obj(
              "type" -> "String",
              "noComment" -> true,
              "isArray" -> false,
              "isEnum" -> false,
              "enumArgs" -> J.arr(),
              "paramType" -> "query",
              "required" -> false
            ),
            "body" -> J.obj(
              "type" -> "User",
              "noComment" -> true,
              "isArray" -> false,
              "isEnum" -> false,
              "enumArgs" -> J.arr(),
              "paramType" -> "body",
              "required" -> true
            )),
          "errors" -> J.arr(
            J.obj(
              "code" -> 400,
              "message" -> "What?"),
            J.obj(
              "code" -> 401,
              "message" -> "Syntax Error")
          ),
          "results" -> J.obj(
            "203" -> J.obj(
              "type" -> "User",
              "isArray" -> false,
              "isEnum" -> false,
              "enumArgs" -> J.arr(),
              "comment" -> "Result comment",
              "paramType" -> JNull,
              "required" -> true
            ),
            "405" -> J.obj(
              "type" -> "String",
              "isArray" -> false,
              "isEnum" -> false,
              "enumArgs" -> J.arr(),
              "comment" -> "Error comment",
              "paramType" -> JNull,
              "required" -> true
            )
          )
        ),
        play.api.test.Helpers.running(FakeApplication()) {
          ApiDocParser.getJson(ApiDocSamples.doc1)
        }
      )
    }

    "Check Array result" in {
      val a = no.samordnaopptak.apidoc.ApiDocParser.getJson(ApiDocSamples.docWithArrayResult)
      //println("a: "+J.prettyPrint(a))

      JsonMatcher.matchJson(
        a,
        J.obj(
          "method" -> "GET",
          "uri"    -> "/api/v1/usernames",
          "uriParms" -> J.arr(),
          "shortDescription" -> "Get Usernames",
          "longDescription" -> "",
          "results" -> J.obj(
            "205" -> J.obj(
              "type" -> "String",
              "comment" -> "",
              "isArray" -> true,
              "isEnum" -> false,
              "enumArgs" -> J.arr(),
              "paramType" -> JNull,
              "required" -> true
            )
          )
        )
      )
    }

    "Check doc with Enum parameters and result" in {
      val a = no.samordnaopptak.apidoc.ApiDocParser.getJson(ApiDocSamples.docWithEnums)
      //println("a: "+J.prettyPrint(a))

      JsonMatcher.matchJson(
        a,
        J.obj(
          "method" -> "GET",
          "uri"    -> "/api/v1/username/{id}",
          "uriParms" -> J.arr("id"),
          "shortDescription" -> "Get Username",
          "longDescription" -> "",
          "parameters" -> J.obj(
            "id" -> J.obj(
              "type" -> "String",
              "noComment" -> true,
              "isArray" -> false,
              "isEnum" -> true,
              "enumArgs" -> J.arr("1","2","3"),
              "paramType" -> "path",
              "required" -> true
            ),
            "body" -> J.obj(
              "type" -> "EnumModel",
              "noComment" -> true,
              "isArray" -> false,
              "isEnum" -> false,
              "enumArgs" -> J.arr(),
     //         "enumArgs" -> J.arr("a1","b2","c3"),
              "paramType" -> "body",
              "required" -> true
            )
          ),
          "results" -> J.obj(
            "200" -> J.obj(
              "type" -> "String",
              "comment" -> "",
              "isArray" -> false,
              "isEnum" -> true,
              "enumArgs" -> J.arr("a","b","c","4"),
              "paramType" -> JNull,
              "required" -> true
            )
          )
        )
      )
    }

    "Check datatype with Enum" in {
      val a = no.samordnaopptak.apidoc.ApiDocParser.getDataTypes(ApiDocSamples.docWithEnums)
      //println("a: "+J.prettyPrint(a))

      JsonMatcher.matchJson(
        J.obj(
          "EnumModel" -> J.obj(
            "enumVal" -> J.obj(
              "type" -> "String",
              "noComment" -> true,
              "isArray" -> false,
              "isEnum" -> true,
              "enumArgs" -> J.arr("a1","b2","c3"),
              "paramType" -> "path",
              "required" -> true
            )
          )
        ),
        a.toJson
      )
    }

    "Get datatypes from ApiDoc" in {
      JsonMatcher.matchJson(
        J.obj(
          "User" -> J.obj(
            "id" -> J.obj(
              "type" -> "String",
              "comment" -> "The ID of the user",
              "isArray" -> false,
              "isEnum" -> false,
              "enumArgs" -> J.arr(),
              "paramType" -> "path",
              "required" -> true
            ),
            "attributes" -> J.obj(
              "type" -> "Attributes",
              "noComment" -> true,
              "isArray" -> true,
              "isEnum" -> false,
              "enumArgs" -> J.arr(),
              "paramType" -> "path",
              "required" -> true
            ),
            "something" -> J.obj(
              "type" -> "String",
              "noComment" -> true,
              "isArray" -> false,
              "isEnum" -> false,
              "enumArgs" -> J.arr(),
              "paramType" -> "path",
              "required" -> true
            )
          ),
          "Attributes" -> J.obj(
            "firstAndLast" -> J.obj(
              "type" -> "String",
              "noComment" -> true,
              "isArray" -> true,
              "isEnum" -> false,
              "enumArgs" -> J.arr(),
              "paramType" -> "path",
              "required" -> true
            ),
            "..." -> J.obj(
              "type" -> "etc.",
              "isArray" -> false,
              "isEnum" -> false,
              "enumArgs" -> J.arr(),
              "required" -> false,
              "noComment" -> true,
              "paramType" -> JNull
            )
          )
        ),
        play.api.test.Helpers.running(FakeApplication()) {
          ApiDocParser.getDataTypes(ApiDocSamples.doc1).toJson
        }
      )
    }
  }
}


