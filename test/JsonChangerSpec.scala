package test

import org.specs2.mutable._

import no.samordnaopptak.json._


class JsonChangerSpec extends Specification {

  import JsonChanger._


  "JsonChanger" should {

    "Change number" in {
      JsonMatcher.matchJson(
        JsonChanger(
          50,
          60
        ),
        60
      )
    }

    "Change string" in {
      JsonMatcher.matchJson(
        JsonChanger(
          "a",
          "b"
        ),
        "b"
      )
    }

    "Change boolean" in {
      JsonMatcher.matchJson(
        JsonChanger(
          true,
          false
        ),
        false
      )
    }

    "Maybe" in {
      JsonMatcher.matchJson(
        JsonChanger(
          null,
          Maybe(Func.number.number(_ + 1))
        ),
        JNull
      )

      JsonMatcher.matchJson(
        JsonChanger(
          90,
          Maybe(Func.number.number(_ + 1))
        ),
        91
      )
    }

    "Change object" in {

      JsonMatcher.matchJson(
        JsonChanger(
          J.obj(
            "aaa" -> 50
          ),
          J.obj(
            ___allowOtherFields
          )
        ),
        J.obj(
          "aaa" -> 50
        )
      )


      JsonMatcher.matchJson(
        JsonChanger(
          J.obj(
            "aaa" -> 50
          ),
          J.obj(
            "aaa" -> 60
          )
        ),
        J.obj(
          "aaa" -> 60
        )
      )
    }

    "Remove fields from objects" in {

      JsonMatcher.matchJson(
        JsonChanger(
          J.obj(
            "aaa" -> 50
          ),
          J.obj(
            "aaa" -> ___removeThisField.number
          )
        ),
        J.obj()
      )

      // test wrong type
      JsonMatcher.matchJson(
        JsonChanger(
          J.obj(
            "aaa" -> 50
          ),
          J.obj(
            "aaa" -> ___removeThisField.string
          )
        ),
        J.obj()
      ) must throwA[JsonChangerException]

      JsonMatcher.matchJson(
        JsonChanger(
          J.obj(),
          J.obj(
            "aaa" -> ___removeThisField.any
          )
        ),
        J.obj()
      ) must throwA[JsonChangerException]

      JsonMatcher.matchJson(
        JsonChanger(
          J.obj(),
          J.obj(
            "aaa" -> Maybe(___removeThisField.any)
          )
        ),
        J.obj()
      )

      JsonMatcher.matchJson(
        JsonChanger(
          J.obj(
            "aaa" -> 50
          ),
          J.obj(
            "aaa" -> Maybe(___removeThisField.number)
          )
        ),
        J.obj()
      )

      JsonMatcher.matchJson(
        JsonChanger(
          J.obj(
            "aaa" -> null
          ),
          J.obj(
            "aaa" -> Maybe(50)
          )
        ),
        J.obj(
          "aaa" -> null
        )
      )
    }


    "Add fields to objects" in {

      JsonMatcher.matchJson(
        JsonChanger(
          J.obj(
            "aaa" -> 50
          ),
          J.obj(
            "aaa" -> 90,
            "bbb" ->NewField(100)
          )
        ),
        J.obj(
          "aaa" -> 90,
          "bbb" -> 100
        )
      )

      JsonChanger(
        J.obj(
          "aaa" -> 50,
          "bbb" -> 20
        ),
        J.obj(
          "aaa" -> 90,
          "bbb" -> NewField(100)
        )
      ) must throwA[JsonChangerException]

    }

    "ForceNewField (Same as NewField, except that the field might already exist)" in {
      JsonMatcher.matchJson(
        JsonChanger(
          J.obj(
            "aaa" -> 50,
            "bbb" -> 20
          ),
          J.obj(
            "aaa" -> 90,
            "bbb" -> ForceNewField(100)
          )
        ),
        J.obj(
          "aaa" -> 90,
          "bbb" -> 100
        )
      )

      JsonMatcher.matchJson(
        JsonChanger(
          J.obj(
            "aaa" -> 50
          ),
          J.obj(
            "aaa" -> 90,
            "bbb" -> ForceNewField(100)
          )
        ),
        J.obj(
          "aaa" -> 90,
          "bbb" -> 100
        )
      )

      // The example from scaladoc
      JsonMatcher.matchJson(
        JsonChanger(
          J.obj(
            "aaa" -> "hello"
          ),
          J.obj(
            "aaa" -> ForceNewField(Func.maybeString.string(_.getOrElse("[undefined]"))),
            "bbb" -> ForceNewField(Func.maybeString.string(_.getOrElse("[undefined]")))
          )
        ),
        J.obj(
          "aaa" -> "hello",
          "bbb" -> "[undefined]"
        )
      )

      JsonMatcher.matchJson(
        JsonChanger(
          J.obj(
            "aaa" -> 50,
            "ccc" -> 60
          ),
          J.obj(
            "aaa" -> ForceNewField(Replace(40,100)),
            "bbb" -> ForceNewField(105),
            "ccc" -> ForceNewField(Replace(60,110))
          )
        ),
        J.obj(
          "aaa" -> 50,
          "bbb" -> 105,
          "ccc" -> 110
        )
      )
    }

    "Add elements to arrays" in {
      JsonMatcher.matchJson(
        JsonChanger(
          J.arr(
          ),
          J.arr(
            InsertValue(90)
          )
        ),
        J.arr(
          90
        )
      )
      JsonMatcher.matchJson(
        JsonChanger(
          J.arr(
            2
          ),
          J.arr(
            InsertValue(90),
            ___identity.number
          )
        ),
        J.arr(
          90,
          2
        )
      )
      JsonMatcher.matchJson(
        JsonChanger(
          J.arr(
            2
          ),
          J.arr(
            ___identity.number,
            InsertValue(90)
          )
        ),
        J.arr(
          2,
          90
        )
      )
      JsonMatcher.matchJson(
        JsonChanger(
          J.arr(
            2,
            3
          ),
          J.arr(
            ___identity.number,
            InsertValue(90),
            ___identity.number
          )
        ),
        J.arr(
          2,
          90,
          3
        )
      )
    }

    "Remove elements from arrays" in {
      JsonChanger(
        J.arr(),
        J.arr(
          ___removeValue.any
        )
      ) must throwA[JsonChangerException]

      JsonChanger(
        J.arr(2),
        J.arr(
          ___removeValue.number,
          ___removeValue.any
        )
      ) must throwA[JsonChangerException]

      // test wrong type
      JsonChanger(
        J.arr(2),
        J.arr(
          ___removeValue.string
        )
      ) must throwA[JsonChangerException]

      JsonMatcher.matchJson(
        JsonChanger(
          J.arr(2),
          J.arr(
            ___removeValue.number
          )
        ),
        J.arr()
      )

      JsonMatcher.matchJson(
        JsonChanger(
          J.arr(
            2,
            3
          ),
          J.arr(
            ___removeValue.number,
            ___identity.number
          )
        ),
        J.arr(
          3
        )
      )

      JsonMatcher.matchJson(
        JsonChanger(
          J.arr(
            2,
            3
          ),
          J.arr(
            ___identity.number,
            ___removeValue.number
          )
        ),
        J.arr(
          2
        )
      )

      JsonMatcher.matchJson(
        JsonChanger(
          J.arr(
            2,
            3
          ),
          J.arr(
            ___removeValue.number,
            ___removeValue.number
          )
        ),
        J.arr()
      )

      JsonMatcher.matchJson(
        JsonChanger(
          J.arr(
            2,
            3,
            4
          ),
          J.arr(
            ___identity.number,
            ___removeValue.number,
            ___identity.number
          )
        ),
        J.arr(2,4)
      )
    }

    "Check unused changer keys when changing objects" in {

      JsonChanger(
        J.obj(
        ),
        J.obj(
          "aaa" -> 50
        )
      ) must throwA[JsonChangerException]
    }

    "Check Func" in {

      JsonMatcher.matchJson(
        JsonChanger(
          J.obj(
            "aaa" -> 30
          ),
          J.obj(
            "aaa" -> Func.number.number(_ + 50)
          )
        ),
        J.obj(
          "aaa" -> 80
        )
      )
    }


    "Check Replace in object" in {

      JsonMatcher.matchJson(
        JsonChanger(
          J.obj(
            "aaa" -> 30,
            "bbb" -> 50
          ),
          J.obj(
            "aaa" -> Replace(30, 100),
            "bbb" -> Replace(30, 100)
          )
        ),
        J.obj(
          "aaa" -> 100,
          "bbb" -> 50
        )
      )
    }

    "Check Maybe in object" in {

      JsonMatcher.matchJson(
        JsonChanger(
          J.obj(
            "aaa" -> 30
          ),
          J.obj(
            "aaa" -> Maybe(50)
          )
        ),
        J.obj(
          "aaa" -> 50
        )
      )


      JsonMatcher.matchJson(
        JsonChanger(
          J.obj(
          ),
          J.obj(
            "aaa" -> Maybe(50)
          )
        ),
        J.obj(
        )
      )
    }


    "Check Maybe(Func)" in {

      JsonMatcher.matchJson(
        JsonChanger(
          J.obj(
            "aaa" -> 30
          ),
          J.obj(
            "aaa" -> Maybe(Func.number.number(_ + 50))
          )
        ),
        J.obj(
          "aaa" -> 80
        )
      )


      JsonMatcher.matchJson(
        JsonChanger(
          J.obj(
          ),
          J.obj(
            "aaa" -> Maybe(Func.number.number(_ + 50))
          )
        ),
        J.obj(
        )
      )
    }


    "Check ChangeThisField" in {

      JsonMatcher.matchJson(
        JsonChanger(
          J.obj(
            "aaa" -> 50
          ),
          J.obj(
            "aaa" -> ChangeThisField(
              "bbb" -> Func.number.number(_ + 50)
            )
          )
        ),
        J.obj(
          "bbb" -> 100
        )
      )
    }

    "Check ChangeThisField + Maybe" in {
      JsonMatcher.matchJson(
        JsonChanger(
          J.obj(
            "aaa" -> 50
          ),
          J.obj(
            "aaa" -> ChangeThisField(
              "bbb" -> Maybe(Func.number.number(_ + 50))
            )
          )
        ),
        J.obj(
          "bbb" -> 100
        )
      )

      JsonMatcher.matchJson(
        JsonChanger(
          J.obj(
          ),
          J.obj(
            "aaa" -> ChangeThisField(
              "bbb" -> Maybe(Func.number.number(_ + 50))
            )
          )
        ),
        J.obj(
        )
      )

      JsonMatcher.matchJson(
        JsonChanger(
          J.obj(
            "aaa" -> null
          ),
          J.obj(
            "aaa" -> ChangeThisField(
              "bbb" -> Maybe(Func.number.number(_ + 50))
            )
          )
        ),
        J.obj(
          "bbb" -> null
        )
      )

      JsonMatcher.matchJson(
        JsonChanger(
          J.obj(
            "aaa" -> 20
          ),
          J.obj(
            "aaa" -> ChangeThisField(
              "bbb" -> Maybe(Func.number.number(_ + 50))
            )
          )
        ),
        J.obj(
          "bbb" -> 70
        )
      )
    }

    "Arrays" in {

      JsonMatcher.matchJson(
        JsonChanger(
          J.arr(),
          J.arr()
        ),
        J.arr()
      )

      JsonMatcher.matchJson(
        JsonChanger(
          J.arr(1),
          J.arr(1)
        ),
        J.arr(1)
      )

      JsonMatcher.matchJson(
        JsonChanger(
          J.arr(5),
          J.arr(
            ___allowOtherValues
          )
        ),
        J.arr(5)
      )

      JsonChanger(
        J.obj(
          "hello" -> J.arr(5)
        ),
        J.obj(
          "hello" -> J.arr(
                       ___allowOtherValues,
                       5
                     )
        )
      ) must throwA(new JsonChangerException("Can not have values after ___allowOtherValues in an array: [ 5 ]\n\npath: .hello\n\n ", ".hello"))

      JsonChanger(
        J.arr(5),
        J.arr()
      ) must throwA[JsonChangerException]

      JsonChanger(
        J.arr(),
        J.arr(5)
      ) must throwA[JsonChangerException]

      JsonMatcher.matchJson(
        JsonChanger(
          J.arr(2,3),
          J.arr(3,2)
        ),
        J.arr(3,2)
      )


      JsonMatcher.matchJson(
        JsonChanger(
          J.arr(2,3),
          J.arr(3, Func.number.number(_ + 5))
        ),
        J.arr(3,8)
      )
    }


    "Array Replace" in {

      JsonMatcher.matchJson(
        JsonChanger(
          J.arr(2,3),
          J.arr(Replace(3, 5), Replace(3, 5))
        ),
        J.arr(2, 5)
      )

    }


    "TypeChange type checking" in {

      import Expects._

      def check(legalType: Type, json: Any) = {

        def check_input_type(testType: Type) = {

          if (legalType != testType)
            JsonChanger(
              json,
              TypeChange(testType, String, "hello")
            ) must throwA[JsonChangerException]
          else
            JsonChanger(
              json,
              TypeChange(testType, String, "hello")
            ) === J("hello")

          JsonChanger(
            json,
            TypeChange(Any, String, "hello")
          ) === J("hello")

        }
        
        def check_output_type(testType: Type) = {

          if (legalType != testType)
            JsonChanger(
              "hello",
              TypeChange(String, testType, json)
            ) must throwA[JsonChangerException]
          else
            JsonChanger(
              "hello",
              TypeChange(String, testType, json)
            ) === J(json)

          JsonChanger(
            "hello",
            TypeChange(String, Any, json)
          ) === J(json)

        }
        
        List(Object, Array, Number, Null, String, Boolean).foreach(check_input_type)
        List(Object, Array, Number, Null, String, Boolean).foreach(check_output_type)
      }


      check(Object, J.obj())
      check(Array, J.arr())
      check(Number, 50)
      check(Number, 50.2)
      check(Null, null)
      check(String, "hello")
      check(Boolean, true)


      // Test Defined
      //
      JsonChanger(
        50,
        TypeChange(Defined, String, "hello")
      ) === J("hello")
      
      JsonChanger(
        null,
        TypeChange(Defined, String, "hello")
      ) must throwA[JsonChangerException]
      
      JsonChanger(
        J.obj()("hello"),
        TypeChange(Defined, String, "hello")
      ) must throwA[JsonChangerException]


      // Test Undefined and null
      //
      JsonChanger(
        50,
        TypeChange(Undefined, String, "hello")
      ) must throwA[JsonChangerException]
      
      JsonChanger(
        50,
        TypeChange(Null, String, "hello")
      ) must throwA[JsonChangerException]
      
      JsonChanger(
        null,
        TypeChange(Undefined, String, "hello")
      ) === J("hello")
      
      JsonChanger(
        null,
        TypeChange(Null, String, "hello")
      ) === J("hello")
      
      JsonChanger(
        J.obj()("hello"),
        TypeChange(Undefined, String, "hello")
      ) === J("hello")


      // Test Maybe
      //
      def test_maybe(type_ : Type, legal: Any, illegal: Any) = {
        JsonChanger(
          legal,
          TypeChange(type_, type_, legal)
        ) === J(legal)

        JsonChanger(
          None,
          TypeChange(type_, type_, legal)
        ) === J(legal)

        JsonChanger(
          legal,
          TypeChange(type_, type_, None)
        ) === JNull

        JsonChanger(
          None,
          TypeChange(type_, type_, None)
        ) === JNull

        JsonChanger(
          illegal,
          TypeChange(type_, type_, legal)
        ) must throwA[JsonChangerException]

        JsonChanger(
          legal,
          TypeChange(type_, type_, illegal)
        ) must throwA[JsonChangerException]
      }

      test_maybe(MaybeObject,  J.obj(),  "hello")
      test_maybe(MaybeArray,   J.arr(),  "hello")
      test_maybe(MaybeString,  "hello",  50)
      test_maybe(MaybeNumber,  50,       "hello")
      test_maybe(MaybeBoolean, true,     "hello")
    }


    "Check InputOutputTypeTransformer" in {

      import Expects._

      TypeChange._object._object(50) === TypeChange(Object, Object, 50)
      TypeChange.array.array(50) === TypeChange(Array, Array, 50)
      TypeChange.string.string(50) === TypeChange(String, String, 50)
      TypeChange.number.number(50) === TypeChange(Number, Number, 50)
      TypeChange.boolean.boolean(50) === TypeChange(Boolean, Boolean, 50)

      TypeChange._null._null(50) === TypeChange(Null, Null, 50)
      TypeChange.defined.defined(50) === TypeChange(Defined, Defined, 50)
      TypeChange.undefined.undefined(50) === TypeChange(Undefined, Undefined, 50)

      TypeChange.maybeObject.maybeObject(50) === TypeChange(MaybeObject, MaybeObject, 50)
      TypeChange.maybeArray.maybeArray(50) === TypeChange(MaybeArray, MaybeArray, 50)
      TypeChange.maybeString.maybeString(50) === TypeChange(MaybeString, MaybeString, 50)
      TypeChange.maybeNumber.maybeNumber(50) === TypeChange(MaybeNumber, MaybeNumber, 50)
      TypeChange.maybeBoolean.maybeBoolean(50) === TypeChange(MaybeBoolean, MaybeBoolean, 50)

      TypeChange.any.any(50) === TypeChange(Any, Any, 50)
    }

    "Check type checking + use TypeChange to avoid throwing error" in {

      // obj vs. arr
      JsonChanger(
        J.obj(),
        J.arr()
      ) must throwA[JsonChangerException]


      JsonMatcher.matchJson(
        JsonChanger(
          J.obj(),
          TypeChange(Expects.Object, Expects.Array, J.arr())
        ),
        J.arr()
      )


      // arr vs. obj
      JsonChanger(
        J.arr(),
        J.obj()
      ) must throwA[JsonChangerException]

      JsonMatcher.matchJson(
        JsonChanger(
          J.arr(),
          TypeChange(Expects.Array, Expects.Object, J.obj())
        ),
        J.obj()
      )


      // arr.arr vs arr.obj
      JsonChanger(
        J.arr(J.arr(1)),
        J.arr(J.obj("a" -> 2))
      ) must throwA[JsonChangerException]

      JsonMatcher.matchJson(
        JsonChanger(
          J.arr(J.arr(1)),
          J.arr(TypeChange(Expects.Array, Expects.Object, J.obj("a" -> 2)))
        ),
        J.arr(J.obj("a" -> 2))
      )


      // obj.arr vs. obj.obj
      JsonChanger(
        J.obj("a" -> J.arr(1)),
        J.obj("a" -> J.obj("b" -> 3))
      ) must throwA[JsonChangerException]


      JsonMatcher.matchJson(
        JsonChanger(
          J.obj("a" -> J.arr(1)),
          J.obj("a" -> TypeChange(Expects.Array, Expects.Object, J.obj("b" -> 3)))
        ),
        J.obj("a" -> J.obj("b" -> 3))
      )


      JsonChanger(
        50,
        Func.number.string(_.asInt.toString)
      ) === JString("50")


      // Drop TypeChange checking from here on. It seems to work.


      // number vs. string
      JsonChanger(
        50,
        "aiai"
      ) must throwA[JsonChangerException]



      // string vs. number
      JsonChanger(
        "aiai",
        50
      ) must throwA[JsonChangerException]


      // boolean vs. string
      JsonChanger(
        true,
        "aiai"
      ) must throwA[JsonChangerException]


      // JNull vs. string
      /*
      JsonChanger(
        JNull,
        "aiai"
      ) must throwA[JsonChangerException]
       */
    }


    "Map" in {
      JsonChanger(
        J.obj("a" -> 2),
        Map.number.number(_ + 2)
      ) must throwA[JsonChangerException]

      JsonMatcher.matchJson(
        JsonChanger(
          J.arr(2,3),
          Map.number.number(_ + 9)
        ),
        J.arr(11,12)
      )

      try{
        JsonChanger(
          J.arr(11),
          Map.string.number(_ + 2)
        )
      } catch {
        case e: JsonChangerException =>
          e.getMessage().contains("""expected a value of the type "string", but found 11 instead.""") must beTrue
      }

      try{
        JsonChanger(
          J.arr(11),
          Map.number.string(_ + 2)
        )
      } catch {
        case e: JsonChangerException =>
          e.getMessage().contains("""expected a value of the type "string", but found 13 instead.""") must beTrue
      }

      true
    }


    "MapChanger" in {
      JsonChanger(
        J.obj("a" -> 2),
        MapChanger(50)
      ) must throwA[JsonChangerException]

      JsonMatcher.matchJson(
        JsonChanger(
          J.arr(2,3),
          MapChanger(Replace(3,9))
        ),
        J.arr(2,9)
      )
    }


    "Check that the correct path is printed in JsonChangerException" in {
      try{
        JsonChanger(
          J.obj(
            "aaa" -> J.arr(
              200,
              J.obj(
                "bbb" -> J.arr(2,3,4)
              )
            )
          ),
          J.obj(
            "aaa" -> J.arr(
              200,
              J.obj(
                "bbb" -> J.arr(2,3,Map.number.number(_ + 100))
              )
            )
          )
        )

        throw new Exception("")

      } catch {
        case e: JsonChangerException => {
          e.getMessage().contains(".aaa[1].bbb[2]") must beTrue
        }
      }

      true
    }

    "Identity (the main example in the scala doc)" in {
      val json = J.obj(
        "aaa" -> 50,
        "b" -> 1
      )

      JsonMatcher.matchJson(
        json - "aaa" ++ J.obj(
          "aaa" -> 60
        ),
        JsonChanger(
          json,
          J.obj(
            "aaa" -> 60,
            "b" -> ___identity.number
          )
        )
      )

    }


  }

}

