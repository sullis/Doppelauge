package test

import org.specs2.mutable._
import play.api.test._

import com.google.inject.Inject

import no.samordnaopptak.apidoc.ApiDocValidation



class ApiDocValidationSpec extends Specification with InjectHelper {

  lazy val apiDocValidation = inject[ApiDocValidation]

  class Inner1{
    case class Inner2()
  }

  "ApiDocValidation" should {

    "Get proper error message if trying to load non-existent class" in {
      play.api.test.Helpers.running(FakeApplication()) {
        try{
          apiDocValidation.loadInnerClass("test.ApiDocValidationSpec.Inner1.Inner25")
          throw new Exception("what?")
        } catch {
          case e: apiDocValidation.ClassNotFoundException => e.getMessage().contains("test.ApiDocValidationSpec.Inner1.Inner25") should beTrue
        }
      }
    }

    "Get Class objects from inner classes" in {
      play.api.test.Helpers.running(FakeApplication()) {
        apiDocValidation.loadInnerClass("test.ApiDocValidationSpec.Inner1.Inner2")
        true
      }
    }

    "Validate data type fields, with no added or removed fields" in {
      play.api.test.Helpers.running(FakeApplication()) {
        apiDocValidation.validateDataTypeFields("test.User", "hepp", Set("id", "attributes", "unrelated"),        Set(), Set())
        apiDocValidation.validateDataTypeFields("test.User", "hepp", Set("id"),                                   Set(), Set()) should throwA[apiDocValidation.MismatchFieldException]
        apiDocValidation.validateDataTypeFields("test.User", "hepp", Set("id", "attributes", "unrelated", "id2"), Set(), Set()) should throwA[apiDocValidation.MismatchFieldException]
      }
    }

    "Validate data type fields, with added field" in {
      play.api.test.Helpers.running(FakeApplication()) {
        apiDocValidation.validateDataTypeFields("test.User", "hepp", Set("id", "id2", "attributes", "unrelated"),        Set("id2"), Set())
        apiDocValidation.validateDataTypeFields("test.User", "hepp", Set("id", "id2", "attributes", "unrelated"),        Set("id"),  Set()) should throwA[apiDocValidation.AlreadyDefinedFieldException]
        apiDocValidation.validateDataTypeFields("test.User", "hepp", Set("id", "attributes", "unrelated"),               Set("id2"), Set()) should throwA[apiDocValidation.MismatchFieldException]
        apiDocValidation.validateDataTypeFields("test.User", "hepp", Set("id", "id2", "id3", "attributes", "unrelated"), Set("id2"), Set()) should throwA[apiDocValidation.MismatchFieldException]
      }
    }

    "Validate data type fields, with removed field" in {
      play.api.test.Helpers.running(FakeApplication()) {
        apiDocValidation.validateDataTypeFields("test.User", "hepp", Set("attributes", "unrelated"),              Set(),     Set("id"))
        apiDocValidation.validateDataTypeFields("test.User", "hepp", Set("id","attributes", "unrelated"),         Set("id"), Set("id"))  should throwA[apiDocValidation.AlreadyDefinedFieldException]
        apiDocValidation.validateDataTypeFields("test.User", "hepp", Set("unrelated"),                            Set(),     Set("id"))  should throwA[apiDocValidation.MismatchFieldException]
        apiDocValidation.validateDataTypeFields("test.User", "hepp", Set("id", "attributes", "unrelated"),        Set(),     Set("id2")) should throwA[apiDocValidation.UnknownFieldException]
      }
    }


  }

}


