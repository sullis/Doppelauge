package test

import org.specs2.mutable._

import no.samordnaopptak.json._


class JsonChangerSpec extends Specification {

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
          JsonChanger.Maybe(JsonChanger.Func(_ + 1))
        ),
        JNull
      )

      JsonMatcher.matchJson(
        JsonChanger(
          90,
          JsonChanger.Maybe(JsonChanger.Func(_ + 1))
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
            JsonChanger.___allowOtherFields
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
            "aaa" -> JsonChanger.___removeThisField
          )
        ),
        J.obj()
      )

      JsonMatcher.matchJson(
        JsonChanger(
          J.obj(),
          J.obj(
            "aaa" -> JsonChanger.___removeThisField
          )
        ),
        J.obj()
      ) must throwA[JsonChangerException]

      JsonMatcher.matchJson(
        JsonChanger(
          J.obj(),
          J.obj(
            "aaa" -> JsonChanger.Maybe(JsonChanger.___removeThisField)
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
            "aaa" -> JsonChanger.Maybe(JsonChanger.___removeThisField)
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
            "aaa" -> JsonChanger.Maybe(50)
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
            "bbb" ->JsonChanger.NewField(100)
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
          "bbb" -> JsonChanger.NewField(100)
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
            "bbb" -> JsonChanger.ForceNewField(100)
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
            "bbb" -> JsonChanger.ForceNewField(100)
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
            "aaa" -> JsonChanger.ForceNewField(JsonChanger.Func(_.getOrElse("[undefined]"))),
            "bbb" -> JsonChanger.ForceNewField(JsonChanger.Func(_.getOrElse("[undefined]")))
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
            "aaa" -> JsonChanger.ForceNewField(JsonChanger.Replace(40,100)),
            "bbb" -> JsonChanger.ForceNewField(105),
            "ccc" -> JsonChanger.ForceNewField(JsonChanger.Replace(60,110))
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
            JsonChanger.InsertValue(90)
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
            JsonChanger.InsertValue(90),
            JsonChanger.___identity
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
            JsonChanger.___identity,
            JsonChanger.InsertValue(90)
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
            JsonChanger.___identity,
            JsonChanger.InsertValue(90),
            JsonChanger.___identity
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
          JsonChanger.___removeValue
        )
      ) must throwA[JsonChangerException]

      JsonChanger(
        J.arr(2),
        J.arr(
          JsonChanger.___removeValue,
          JsonChanger.___removeValue
        )
      ) must throwA[JsonChangerException]

      JsonMatcher.matchJson(
        JsonChanger(
          J.arr(2),
          J.arr(
            JsonChanger.___removeValue
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
            JsonChanger.___removeValue,
            JsonChanger.___identity
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
            JsonChanger.___identity,
            JsonChanger.___removeValue
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
            JsonChanger.___removeValue,
            JsonChanger.___removeValue
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
            JsonChanger.___identity,
            JsonChanger.___removeValue,
            JsonChanger.___identity
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

    "Check JsonChanger.Func" in {

      JsonMatcher.matchJson(
        JsonChanger(
          J.obj(
            "aaa" -> 30
          ),
          J.obj(
            "aaa" -> JsonChanger.Func(_ + 50)
          )
        ),
        J.obj(
          "aaa" -> 80
        )
      )
    }


    "Check JsonChanger.Replace in object" in {

      JsonMatcher.matchJson(
        JsonChanger(
          J.obj(
            "aaa" -> 30,
            "bbb" -> 50
          ),
          J.obj(
            "aaa" -> JsonChanger.Replace(30, 100),
            "bbb" -> JsonChanger.Replace(30, 100)
          )
        ),
        J.obj(
          "aaa" -> 100,
          "bbb" -> 50
        )
      )
    }

    "Check JsonChanger.Maybe in object" in {

      JsonMatcher.matchJson(
        JsonChanger(
          J.obj(
            "aaa" -> 30
          ),
          J.obj(
            "aaa" -> JsonChanger.Maybe(50)
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
            "aaa" -> JsonChanger.Maybe(50)
          )
        ),
        J.obj(
        )
      )
    }


    "Check JsonChanger.Maybe(JsonChanger.Func)" in {

      JsonMatcher.matchJson(
        JsonChanger(
          J.obj(
            "aaa" -> 30
          ),
          J.obj(
            "aaa" -> JsonChanger.Maybe(JsonChanger.Func(_ + 50))
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
            "aaa" -> JsonChanger.Maybe(JsonChanger.Func(_ + 50))
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
            "aaa" -> JsonChanger.ChangeThisField("bbb" -> JsonChanger.Func(_ + 50))
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
            "aaa" -> JsonChanger.ChangeThisField(
              "bbb" -> JsonChanger.Maybe(JsonChanger.Func(_ + 50))
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
            "aaa" -> JsonChanger.ChangeThisField(
              "bbb" -> JsonChanger.Maybe(JsonChanger.Func(_ + 50))
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
            "aaa" -> JsonChanger.ChangeThisField(
              "bbb" -> JsonChanger.Maybe(JsonChanger.Func(_ + 50))
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
            "aaa" -> JsonChanger.ChangeThisField(
              "bbb" -> JsonChanger.Maybe(JsonChanger.Func(_ + 50))
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
            JsonChanger.___allowOtherValues
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
                       JsonChanger.___allowOtherValues,
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
          J.arr(3, JsonChanger.Func(_ + 5))
        ),
        J.arr(3,8)
      )
    }


    "Array Replace" in {

      JsonMatcher.matchJson(
        JsonChanger(
          J.arr(2,3),
          J.arr(JsonChanger.Replace(3, 5), JsonChanger.Replace(3, 5))
        ),
        J.arr(2, 5)
      )

    }


    "TypeChange type checking" in {

      import JsonChanger.Expects
      import JsonChanger.Expects._

      def check(legalType: Expects.Type, json: Any) = {

        def check2(testType: Expects.Type) = {

          if (legalType != testType)
            JsonChanger(
              json,
              JsonChanger.TypeChange(testType, "hello")
            ) must throwA[JsonChangerException]
          else
            JsonChanger(
              json,
              JsonChanger.TypeChange(testType, "hello")
            ) === J("hello")

          JsonChanger(
            json,
            JsonChanger.TypeChange(Anything, "hello")
          ) === J("hello")

        }
        
        List(Object, Array, Number, Null, String, Boolean).foreach(check2)
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
        JsonChanger.TypeChange(Defined, "hello")
      ) === J("hello")
      
      JsonChanger(
        null,
        JsonChanger.TypeChange(Defined, "hello")
      ) must throwA[JsonChangerException]
      
      JsonChanger(
        J.obj()("hello"),
        JsonChanger.TypeChange(Defined, "hello")
      ) must throwA[JsonChangerException]


      // Test Undefined
      //
      JsonChanger(
        50,
        JsonChanger.TypeChange(Undefined, "hello")
      ) must throwA[JsonChangerException]
      
      JsonChanger(
        null,
        JsonChanger.TypeChange(Undefined, "hello")
      ) === J("hello")
      
      JsonChanger(
        J.obj()("hello"),
        JsonChanger.TypeChange(Undefined, "hello")
      ) === J("hello")

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
          JsonChanger.TypeChange(JsonChanger.Expects.Object, J.arr())
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
          JsonChanger.TypeChange(JsonChanger.Expects.Array, J.obj())
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
          J.arr(JsonChanger.TypeChange(JsonChanger.Expects.Array, J.obj("a" -> 2)))
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
          J.obj("a" -> JsonChanger.TypeChange(JsonChanger.Expects.Array, J.obj("b" -> 3)))
        ),
        J.obj("a" -> J.obj("b" -> 3))
      )


      JsonChanger(
        50,
        JsonChanger.TypeChange(JsonChanger.Expects.Number, JsonChanger.Func(_.asInt.toString))
      ) === JString("50")

      JsonChanger(
        50,
        JsonChanger.TypeChange(
          JsonChanger.Expects.Number,
          JsonChanger.Func(_.asInt.toString)
        )
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
        JsonChanger.Map(_ + 2)
      ) must throwA[JsonChangerException]

      JsonMatcher.matchJson(
        JsonChanger(
          J.arr(2,3),
          JsonChanger.Map(_ + 2)
        ),
        J.arr(4,5)
      )
    }


    "MapChanger" in {
      JsonChanger(
        J.obj("a" -> 2),
        JsonChanger.MapChanger(50)
      ) must throwA[JsonChangerException]

      JsonMatcher.matchJson(
        JsonChanger(
          J.arr(2,3),
          JsonChanger.MapChanger(JsonChanger.Replace(3,9))
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
                "bbb" -> J.arr(2,3,JsonChanger.Map(_ + 100))
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
            "b" -> JsonChanger.___identity
          )
        )
      )

    }


  }

}

