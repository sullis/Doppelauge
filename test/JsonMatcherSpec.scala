package test

import org.specs2.mutable._

import play.api.libs.json._
import play.api.test.Helpers._

import no.samordnaopptak.json.J
import no.samordnaopptak.json.JsonMatcher._



class JsonMatcherSpec extends Specification {
  "Json matcher" should {


    // empty tests

    "match empty obj" in {
      matchJson(Json.obj(),Json.obj())
    }

    "match non-empty objs" in {
      matchJson(Json.obj(),Json.obj("name" -> "value")) should throwA[JsonMatcherException]
    }

    "match empty array" in {
      matchJson(Json.arr(),Json.arr())
    }

    "match non-empty array" in {
      matchJson(Json.arr(),Json.arr(Json.arr())) should throwA[JsonMatcherException]
    }



    // simple obj tests

    "match obj with string" in {
      matchJson(
        Json.obj("a" -> "b"),
        Json.obj("a" -> "b")
      )
    }

    "match obj with diff key string" in {
      matchJson(
        Json.obj("a" -> "b"),
        Json.obj("c" -> "b")
      ) should throwA[JsonMatcherException]
    }

    "match obj with diff value string" in {
      matchJson(
        Json.obj("a" -> "b"),
        Json.obj("a" -> "c")
      ) should throwA[JsonMatcherException]
    }


    "match anyString, anyNumber, and so forth" in {
      // string
      matchJson(
        Json.obj("a" -> ___anyString),
        Map("a" -> "b")
      )
      matchJson(
        Json.obj("a" -> ___anyString),
        Json.obj("a" -> 5)
      ) should throwA[JsonMatcherException] 

      // number
      matchJson(
        Json.obj("a" -> ___anyNumber),
        Json.obj("a" -> 5)
      )
      matchJson(
        Json.obj("a" -> ___anyNumber),
        Json.obj("a" -> "b")
      ) should throwA[JsonMatcherException]

      // object
      matchJson(
        Json.obj("a" -> ___anyObject),
        Json.obj("a" -> Json.obj())
      )
      matchJson(
        Json.obj("a" -> ___anyObject),
        Json.obj("a" -> 5)
      ) should throwA[JsonMatcherException] 

      // array
      matchJson(
        Json.obj("a" -> ___anyArray),
        Json.obj("a" -> Json.arr())
      )
      matchJson(
        Json.obj("a" -> ___anyArray),
        Json.obj("a" -> 5)
      ) should throwA[JsonMatcherException] 

      // boolean
      matchJson(
        Json.obj("a" -> ___anyBoolean),
        Json.obj("a" -> true)
      )
      matchJson(
        Json.obj("a" -> ___anyBoolean),
        Json.obj("a" -> 5)
      ) should throwA[JsonMatcherException] 
   }


    "match regexp strings" in {
      matchJson(
        J.obj("a" -> RegExp("b")),
        Json.obj("a" -> "b")
      )

      matchJson(
        J.obj("a" -> RegExp("b")),
        Json.obj("a" -> "cbh")
      )

      matchJson(
        J.obj("a" -> RegExp("^b")),
        Json.obj("a" -> "bh")
      )

      matchJson(
        J.obj("a" -> RegExp("b")),
        Json.obj("a" -> "c")
      ) should throwA[JsonMatcherException] 

      matchJson(
        J.obj("a" -> RegExp("^b")),
        Json.obj("a" -> "cbh")
      ) should throwA[JsonMatcherException] 

      matchJson(
        J.obj("a" -> RegExp("^b$")),
        Json.obj("a" -> "cbh")
      ) should throwA[JsonMatcherException] 

      matchJson(
        J.obj("a" -> RegExp("^b$")),
        Json.obj("a" -> "b")
      )

      matchJson(
        RegExp("^b$"),
        JsString("b")
      )
    }


    "match Or" in {
      matchJson(
        Or("a", 50, 30),
        "a"
      )
      matchJson(
        Or("a", 50),
        50
      )
      matchJson(
        Or("a", 2, 50),
        2
      )
      matchJson(
        Or("a", 50),
        false
      ) should throwA[JsonMatcherException]
      matchJson(
        Or(),
        false
      ) should throwA[JsonMatcherException]
    }

    "match And" in {
      matchJson(
        And(),
        "a"
      )
      matchJson(
        And(___anyNumber, 50),
        JsNumber(50)
      )
      matchJson(
        And("a", 50),
        50
      ) should throwA[JsonMatcherException]
      matchJson(
        And(50, "a"),
        50
      ) should throwA[JsonMatcherException]
    }

    "Creating custom matchers" in {

      // successfull
      matchJson(
        Custom(_ => true),
        JsNull
      )

      matchJson(
        Custom(_ => false),
        JsNull
      ) should throwA[JsonMatcherException]

      matchJson(
        Custom(_.asNumber > 0),
        50
      )

      matchJson(
        Custom(_.asNumber > 0),
        0
      ) should throwA[JsonMatcherException]


      // override toString
      try {
        matchJson(
          new Custom(_ => false){
            override def toString = "gakkgakk"
          },
          JsNull
        )
        throw new Exception("somethingswrong")
      } catch {
        case e: JsonMatcherException =>
          e.getMessage().contains("gakkgakk") must beTrue
      }


      // custom function name (for use in error message)
      try {
        matchJson(
          new Custom(_ => false, name="gakkgakk"),
          JsNull
        )
        throw new Exception("somethingswrong")
      } catch {
        case e: JsonMatcherException =>
          e.getMessage().contains("gakkgakk") must beTrue
      }

      // check that exceptions thrown inside the custom matcher function are handled properly
      // We do this to get more informative error messages and avoid having to check for types.
      try {
        matchJson(
          Custom(_.asNumber > 0),
          "gakkgakkgakk"
        )
        throw new Exception("somethingswrong")
      } catch {
        case e: JsonMatcherException => {
          e.getMessage().contains("gakkgakkgakk") must beTrue
          e.getMessage().contains("threw an exception") must beTrue
        }
      }

    }


    "match Maybe" in {

      matchJson(
        J.obj(
          "a" -> Maybe(1)
        ),
        Json.obj(
          "a" -> 1
        )
      )

      matchJson(
        J.obj(
          "a" -> Maybe(1)
        ),
        Json.obj(
          "a" -> JsNull
        )
      )

      matchJson(
        J.obj(
          "a" -> Maybe(1)
        ),
        Json.obj()
      )

      matchJson(
        J.obj(
          "a" -> Maybe(2)
        ),
        Json.obj(
          "a" -> 1
        )
      ) must throwA[JsonMatcherException]

      matchJson(
        J.obj(
          "a" -> Maybe(___anyNumber)
        ),
        Json.obj()
      )

      matchJson(
        J.obj(
          "a" -> Maybe(___anyNumber)
        ),
        Json.obj(
          "a" -> 50
        )
      )

      matchJson(
        J.obj(
          "a" -> Maybe(___anyNumber)
        ),
        Json.obj(
          "a" -> "c"
        )
      ) must throwA[JsonMatcherException]
    }

    // simple array tests

    "match obj with array" in {
      matchJson(
        Json.obj("a" -> Json.arr()),
        Json.obj("a" -> Json.arr())
      )
    }

    "match obj with array" in {
      matchJson(
        Json.obj("a" -> Json.arr()),
        Json.obj("a" -> Json.arr())
      )
    }

    "match something else" in {
      matchJson(
        Json.obj("data" -> Json.arr(Json.obj("name" -> "ARole"))),
        Json.obj("data" -> Json.arr(Json.obj("name" -> "ARole")))
      )
    }



    // simple array/obj mix tests

    "match obj with array doesnt match" in {
      matchJson(
        Json.obj(),
        Json.arr()
      ) should throwA[JsonMatcherException]
    }



    // other fields, object

    "match object with other fields" in {
      matchJson(
        Json.obj(___allowOtherFields),
        Json.obj()
      )
    }

    "match object with other fields 2" in {
      matchJson(
        Json.obj(___allowOtherFields),
        Json.obj("a" -> "b")
      )
    }

    "match object with other fields 3" in {
      matchJson(
        Json.obj("hepp" -> "aiai", ___allowOtherFields),
        Json.obj("hepp" -> "aiai", "b" -> "c")
      )
    }

    "match object with diff fields, containing others" in {
      matchJson(
        Json.obj("hepp" -> "aiai", ___allowOtherFields),
        Json.obj("b" -> "c")
      ) should throwA[JsonMatcherException]
    }



    // other fields, array

    "match array with other fields 1" in {
      matchJson(
        Json.arr(___allowOtherValues),
        Json.arr()
      )
    }

    "match array with other fields 2" in {
      matchJson(
        Json.arr(___allowOtherValues),
        Json.arr("a")
      )
    }

    "match arr with other fields 3" in {
      matchJson(
        Json.arr("hepp", ___allowOtherValues),
        Json.arr("hepp", "b")
      )
    }

    "match array with diff fields, containing others" in {
      matchJson(
        Json.arr("hepp", ___allowOtherValues),
        Json.arr("b")
      ) should throwA[JsonMatcherException]
    }



    // num field, array

    "match array with num field" in {
      matchJson(
        Json.arr(___numElements, 0),
        Json.arr()
      )
    }


    "fail match array with num field, when too few elements" in {
      matchJson(
        Json.arr(___numElements, 1),
        Json.arr()
      ) should throwA[JsonMatcherException]
    }

    "fail match array with num field, when too many elements" in {
      matchJson(
        Json.arr(___numElements, 1),
        Json.arr("a","b","c")
      ) should throwA[JsonMatcherException]
    }


    "match array with num field 2" in {
      matchJson(
        Json.arr("hello",___numElements, 1),
        Json.arr("hello")
      )
    }

    "match array with num field 2b" in {
      matchJson(
        Json.arr(___numElements, 1),
        Json.arr("hello")
      )
    }

    "fail match array with num field 2" in {
      matchJson(
        Json.arr("hello", ___numElements, 3),
        Json.arr("hello1", "hello2", "hello3")
      ) should throwA[JsonMatcherException]
    }
 
    "match array with num field 3" in {
      matchJson(
        Json.arr(___numElements, 1, "hello"),
        Json.arr("hello")
      )
    }

    "match array with num field 4" in {
      matchJson(
        Json.arr("a", ___numElements, 2, "hello"),
        Json.arr("a", "hello")
      )
    }

    "match array with num field 4b" in {
      matchJson(
        Json.arr(___numElements, 4),
        Json.arr("a", "hello", "b", "c")
      )
    }


    // num field, object

    "match object with num field" in {
      matchJson(
        Json.obj(___numElements -> 0),
        Json.obj()
      )
    }

    "match object with num field 2" in {
      matchJson(
        Json.obj("hello" -> 5,___numElements -> 1),
        Json.obj("hello" -> 5)
      )
    }

    "match object with num field 2b" in {
      matchJson(
        Json.obj(___numElements -> 1),
        Json.obj("hello" -> 5)
      )
    }

    "fail match object with num field, too few" in {
      matchJson(
        Json.obj(___numElements -> 1),
        Json.obj()
      ) should throwA[JsonMatcherException]
    }

    "fail match object with num field, too many" in {
      matchJson(
        Json.obj(___numElements -> 0),
        Json.obj("hello" -> 5)
      ) should throwA[JsonMatcherException]
    }

    "fail to match array with wrong order" in {
      matchJson(
        Json.arr(2,3),
        Json.arr(3,2)
      ) should throwA[JsonMatcherException]

      matchJson(
        Json.arr(2,3,___allowOtherValues),
        Json.arr(3,2)
      ) should throwA[JsonMatcherException]

      matchJson(
        Json.arr(),
        Json.arr(3)
      ) should throwA[JsonMatcherException]
    }

    "match array with wrong order, but legal anyway because of ___allowOtherValues" in {
      matchJson(
        Json.arr(2,3,___allowOtherValues),
        Json.arr(9,2,3)
          //     ^
          //     ignored
      )
    }

    "match array with wrong order deep down. ___ignoreOrder at toplevel doesn't change that" in {
      matchJson(
        Json.arr(2,Json.arr(8,9),5, ___ignoreOrder),
        Json.arr(2,Json.arr(9,8),5)
      ) should throwA[JsonMatcherException]
    }

    "match array with wrong order, using ___ignoreOrder to match anyway" in {
      matchJson(
        Json.arr(2,3,5, ___ignoreOrder),
        Json.arr(2,3,5)
      )

      matchJson(
        Json.arr(2,3,5),
        Json.arr(2,3,5, ___ignoreOrder)
      )

      matchJson(
        Json.arr(2,3,5, ___ignoreOrder),
        Json.arr(2,3,5, ___ignoreOrder)
      )
    }

    "match array with wrong order, but legal anyway because of ignoreArrayOrder option" in {
      matchJson(
        Json.arr(2,3,5),
        Json.arr(5,2,3),
        ignoreArrayOrder = true
      )
    }

    "match array with wrong order deep down, but legal anyway because of ignoreArrayOrder option (which works recursively)" in {
      matchJson(
        Json.arr(2,Json.arr(8,9),5),
        Json.arr(2,Json.arr(9,8),5),
        ignoreArrayOrder = true
      )
    }

    "fail to match array with several similar elements" in {
      matchJson(
        Json.arr(2,3,5, ___ignoreOrder),
        Json.arr(2,3,5,5)
      ) should throwA[JsonMatcherException]

      matchJson(
        Json.arr(2,3,5),
        Json.arr(2,3,5,5)
      ) should throwA[JsonMatcherException]

      matchJson(
        Json.arr(2,3,5,5, ___ignoreOrder),
        Json.arr(2,3,5)
      ) should throwA[JsonMatcherException]

      matchJson(
        Json.arr(2,3,5),
        Json.arr(2,3,5,5)
      ) should throwA[JsonMatcherException]
    }

    // array and objects tests

    "match arrays and objects, etc." in {
      matchJson(
        Json.obj("data"      -> Json.arr(Json.obj("name"          -> "ARole",
                                                  "numUsers"      -> 1,
                                                  "actions"       -> Json.arr(___allowOtherValues),
                                                   ___numElements -> 4),
                                         ___numElements, 5),
                 "hitsTotal" -> 1),
        Json.obj("data"      -> Json.arr(Json.obj("name"     -> "ARole",
                                                  "numUsers" -> 1,
                                                  "somethingelse" -> Json.arr("hello"),
                                                  "actions"  -> Json.arr("a","b",Json.obj("a" -> "b"))),
                                         "d","e","f",Json.arr("f","g")),
                 "hitsTotal" -> 1)
      )

    }

    "Error message for missing fields must contain the names of the extra fields" in {
      try{
        matchJson(
          Json.obj(
            "a" -> 1
          ),
          Json.obj(
            "a" -> 1,
            "b_extrafield1" -> 3,
            "c_extrafield2" -> 4
          )
        )
        throw new Exception("somethingswrong")
      } catch {
        case e: JsonMatcherException => {
          e.getMessage().contains("missing: b_extrafield1, c_extrafield2") must beTrue
        }
      }
    }

    "Error message for extra fields must contain the names of missing fields" in {
      try{
        matchJson(
          Json.obj(
            "a" -> 1,
            "b_extrafield1" -> 3,
            "c_extrafield2" -> 4
          ),
          Json.obj(
            "a" -> 1
          )
        )
        throw new Exception("somethingswrong")
      } catch {
        case e: JsonMatcherException => {
          e.getMessage().contains("added: b_extrafield1, c_extrafield2") must beTrue
        }
      }
    }

    "Error message must contain the path" in {
      try{
        matchJson(
          Json.obj(
            "a" -> Json.obj(
              "b" -> 3
            )
          ),
          Json.obj(
            "a" -> Json.obj(
            "b" -> 4
            )
          )
        )
        throw new Exception("somethingswrong")
      } catch {
        case e: JsonMatcherException => {
          e.path === "a.b"
          e.getMessage().contains("a.b") must beTrue
        }
      }
    }

    "Error message must contain the path, more extensive test" in {
      try{
        matchJson(
          Json.obj(
            "a" -> Json.obj(
              "b" -> Json.arr(
                1,
                Json.obj(
                  "c" -> Json.obj(
                    "d" -> 1
                  )
                )
              )
            )
          ),
          Json.obj(
            "a" -> Json.obj(
              "b" -> Json.arr(
                1,
                Json.obj(
                  "c" -> Json.obj(
                    "d" -> 2
                  )
                )
              )
            )
          )
        )
        throw new Exception("somethingswrong")
      } catch {
        case e: JsonMatcherException => {
          //println("message: "+e.getMessage())
          e.path === "a.b(1).c.d"
          e.getMessage().contains("a.b(1).c.d") must beTrue
          true
        }
      }
    }


  }

}
