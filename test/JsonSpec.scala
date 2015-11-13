package test

import org.specs2.mutable._

import play.api.libs.{json => pjson}
import play.api.libs.json.{Json => PJson} // i.e. Play Json

import no.samordnaopptak.json._

import no.samordnaopptak.test.TestByAnnotation


class JsonSpec extends Specification {

  "J" should {

    "J.obj" in {

      J.obj().asJsObject === PJson.obj()

      J.obj(
        "a" -> "b"
      ).asJsObject === PJson.obj(
        "a" -> "b"
      )

      J.obj(
        "a" -> 1,
        "b" -> 1.2,
        "c" -> 5134134L
      ).asJsObject === PJson.obj(
        "a" -> 1,
        "b" -> 1.2,
        "c" -> 5134134L
      )

      J.obj(
        "a" -> true
      ).asJsObject === PJson.obj(
        "a" -> true
      )

      J.obj(
        "a" -> PJson.arr(0)
      ).asJsObject === PJson.obj(
        "a" -> PJson.arr(0)
      )

      J.obj(
        "a" -> J.obj("c" -> "d")
      ).asJsObject === PJson.obj(
        "a" -> PJson.obj("c" -> "d")
      )

      true
    }

    "J.arr" in {

      J.arr().asJsValue === PJson.arr()

      J.arr(
        1, "2", false
      ).asJsValue === PJson.arr(
        1, "2", false
      )

      J.arr(
        1,
        "2",
        J.obj(
          "a" -> J.arr(50)
        )
      ).asJsValue === PJson.arr(
        1,
        "2",
        PJson.obj(
          "a" -> PJson.arr(50)
        )
      )

    }

    "Throw JsonParseException if trying to parse illegal json" in {
      J.parse("{]") must throwA[JsonParseException]
      J.parse(""" { "gakk": 50 "o:" } """) must throwA[JsonParseException]
    }


    val numbers = "[2, 0.0, -4.2]"
    val jsonString = s"""
        {
          "longs": $numbers,

          "mixed": [2, "adsf", {"adsf":5}, [2,3]],

          "inner": {
            "longs2": $numbers
          },

          "astring": "astring, definitely",

          "adouble": 2.3,
          "anint": 5,

          "atrueboolean": true,
          "afalseboolean": false,

          "anull": null,

          "anobjectarray": [
            {
              "a": "a_"
            },
            {
              "b": "b_"
            }
          ]
        }
      """

    val json = J.parse(jsonString)

    "Ignore null values in objects." in {
      json("anull") === JNull
      json("anull").isNull === true
      json("anull").asOption(_.asInt) must beNone
      json("anull").asOption(_.asLong) must beNone
      json("anull").asOption(_.asDouble) must beNone
      json("anull").asOption(_.asBoolean) must beNone
      json("anull").asOption(_.asMap) must beNone
      json("anull").asOption(_.asLong) must beNone
      json("anull").asOption(_.asLongArray) must beNone
    }

    "Array of numbers" in {
      json("longs").asLongArray must equalTo(List[Long](2L,0L,-4L))
      json("longs").asIntArray must equalTo(List[Int](2,0,-4))
      json("longs").asStringArray must throwA[JsonException]

      json("mixed").asIntArray must throwA[JsonException]
      json("mixed").asLongArray must throwA[JsonException]
      json("mixed").asStringArray must throwA[JsonException]
    }


    "inner access" in {
      json("inner")("longs2").asLongArray must equalTo(List[Long](2L,0L,-4L))
      json("inner")("longs2").asIntArray must equalTo(List[Int](2,0,-4))
      json("inner")("longs2").asDoubleArray must equalTo(List[Double](2,0,-4.2))
    }

    "strings" in {
      json("astring").asString must equalTo("astring, definitely")
      json("nothinghereastring").asString must throwA[JsonException]
      json("astring").asDoubleArray must throwA[JsonException]
    }


    "numbers" in {
      json("adouble").asDouble must equalTo(2.3)
      json("anint").asDouble must equalTo(5.0)
      json("anint").asInt must equalTo(5)
      json("anint").asLong must equalTo(5L)
      json("anint").asString must throwA[JsonException]
    }

    "booleans" in {
      json("atrueboolean").asBoolean must beTrue
      json("afalseboolean").asBoolean must beFalse
      json("atrueboolean").asInt must throwA[JsonException]
      json("atrueboolean").asString must throwA[JsonException]
    }

    "unknowns" in {
      json("nothere").isInstanceOf[JUndefined] must beTrue
      json("atrueboolean") // check that it's there.
      json("atrueboolean")("gakk") must throwA[JsonException] // wrong type. gakk is a string, not an object.
    }

    "getOrElse" in {
      json("nothere").isInstanceOf[JUndefined] must beTrue
      json("nothere").getOrElse(_.asLong, 50) === 50
      json("adouble").getOrElse(_.asDouble, 50) === 2.3
    }

    "array" in {
      json(0) must throwA[JsonException]
      json(-1) must throwA[JsonException]

      json("anobjectarray")(-1) must throwA[JsonException] // too low
      json("anobjectarray")(2) must throwA[JsonException] // too high
    }

    "multiple types" in {
      json("mixed")(2)("adsf").asDouble must equalTo(5.0)
      json("anobjectarray")(0)("a").asString must equalTo("a_")
      json("anobjectarray")(1)("b").asString must equalTo("b_")
    }

    "Json.asMap" in {
      json.asMap.get("asdf") must beNone
      json.asMap.keySet.contains("anobjectarray") must beTrue
      json.asMap("anobjectarray")(0)("a").asString must equalTo("a_")
      json("astring").asMap must throwA[JsonException]
    }

    "Optional values" in {
      json("longs").asOption(_.asLongArray) must equalTo(Some(List[Long](2L,0L,-4L)))
      json("longs").asOption(_.asString) must throwA[JsonException]
      json("inner").asOption(_.asMap) must beSome
      json("adouble").asOption(_.asDouble) must equalTo(Some(2.3))
      json("anint").asOption(_.asInt) must equalTo(Some(5))
      json("atrueboolean").asOption(_.asBoolean) must equalTo(Some(true))
      json("afalseboolean").asOption(_.asBoolean) must equalTo(Some(false))
      json("astring").asOption(_.asString) must equalTo(Some("astring, definitely"))
    }

    "J from map" in {
      J(
        Map(
          "a" -> 50,
          "b" -> 60
        )
      ).asJsObject === play.api.libs.json.Json.obj(
        "a" -> 50,
        "b" -> 60
      )
    }

    "Json.size/Json.keys/Json.++" in {
      val o = J(
        play.api.libs.json.Json.obj(
          "a" -> 50,
          "b" -> 60
        )
      )
      val a = J(
        play.api.libs.json.Json.arr("a","b","c")
      )

      "Json.size" in {
        o.size must equalTo(2)
        a.size must equalTo(3)
      }

      "Json.keys and Json.++" in {
        o.keys must equalTo(Set("a","b"))
        a.keys must throwA[JsonException]
      }

      "Json.++ (1)" in{
        (J.obj() ++ J.obj()) must equalTo(J.obj())

        val j1 = J.obj("a" -> 1)
        val j2 = J.obj("b" -> 2)
        val j12 = J.obj("a" -> 1, "b" -> 2)
        val j21 = J.obj("b" -> 2, "a" -> 1)

        j12 must equalTo(j21)

        (J.obj() ++ j1) must equalTo(j1)
        (j1 ++ J.obj()) must equalTo(j1)

        (j1 ++ j2) must equalTo(j12)
        (j2 ++ j1) must equalTo(j12)

        (j1 ++ J.obj("a"->2)) must throwA[JsonMergeObjectsException]
      }

      "Json.++ (2)" in {
        (json ++ J.obj()) must equalTo(json)

        (json ++ json) must throwA[JsonMergeObjectsException]
        (o ++ o) must throwA[JsonMergeObjectsException]

        (json ++ o).size must equalTo(json.size + o.size)

        (a ++ o) must throwA[JsonIllegalConversionException]
        (o ++ a) must throwA[JsonIllegalConversionException]
      }
    }

    "JObject.-" in {
      val j = J.obj("hello" -> 1, "hello2" -> 2)

      (j - "hello") must equalTo(J.obj("hello2" -> 2))

      (j - "hello" - "hello2") must equalTo(J.obj())
    }

    "JObject.equal" in {
      J.obj("a" -> 1, "b" -> 2) must equalTo(J.obj("a" -> 1, "b" -> 2))
      J.obj("a" -> 1, "b" -> 2) must equalTo(J.obj("b" -> 2, "a" -> 1))
      J.obj("a" -> 1, "b" -> 2) must not equalTo(J.obj("a" -> 1, "b" -> 3))
    }

    "Ensure error messages contain the undefined field name" in {
      val json = J.obj()
      val result = json("hepp")
      try {
        result("happ")
        throw new Exception("nope")
      } catch {
        case e: JsonIllegalConversionException => {
          e.message.contains("hepp") must beTrue
        }
      }
      true
    }

    "parseIt" in {

      val jsonText = """
          {
            "a" : "a_",
            "b" : "b_"
          }
        """

      J.parseIt(jsonText) { json =>
        true
      } === true

      J.parseIt(jsonText) { json =>
        json("a").asString
      } === "a_"
    }

    "isDefined and hasKey" in {
      val jsonText = """
          {
            "a" : "a_",
            "b" : null
          }
        """

      val json = J.parse(jsonText)

      json("a").isDefined === true
      json("b").isDefined === false
      json("c").isDefined === false

      json.hasKey("a") === true
      json.hasKey("b") === true
      json.hasKey("c") === false
    }

    "ensure order is kept in objects" in {
      val json1 = J.obj(
        "a" -> 1,
        "b" -> 2,
        "c" -> 3
      )

      J.parse(json1.pp()).pp() === json1.pp()

      val json2 = J.obj(
        "d" -> 4,
        "e" -> 5,
        "f" -> 6
      )

      (json1 ++ json2).pp() === J.obj(
        "a" -> 1,
        "b" -> 2,
        "c" -> 3,
        "d" -> 4,
        "e" -> 5,
        "f" -> 6
      ).pp()

      (json1 ++ json2).pp() === J(
        PJson.obj(
          "a" -> 1,
          "b" -> 2,
          "c" -> 3,
          "d" -> 4,
          "e" -> 5,
          "f" -> 6
        )
      ).pp()

      (json1 ++ json2).pp() === J(
        play.api.libs.json.JsObject(
          Seq(
            "a" -> PJson.toJson(1),
            "b" -> PJson.toJson(2),
            "c" -> PJson.toJson(3),
            "d" -> PJson.toJson(4),
            "e" -> PJson.toJson(5),
            "f" -> PJson.toJson(6)
          )
        )
      ).pp()

    }

    "pick / getRecursively" in {

      val text = """
{
 "result" : {
   "name": "navn",
   "data": [
     {
       "language": "slovac",
       "numb": 5,
       "ai": {"ai2": 5, "ai3": {"ai4":11}, "ai5": {"ai4":13}}
     },
     {
       "language": "liber",
       "numb": 6,
       "ai": {"ai2": 6, "ai3": {"ai4":12}}
     },
     {
       "language": "sumi",
       "numb": 7
     }
   ]
 }
}
"""

      val json = J.parse(text)

      json.pick("result" -> "name").asString === "navn"
      json.pick("result" -> "data" -> 0 -> "language").asString === "slovac"
      json.pick("result" -> "data" -> 0 -> "numb").asNumber == 50

      json.pick("result" -> "numb (recursively)").asIntArray == Seq(5,6,7)
      json.pick("numb (recursively)").asIntArray == Seq(5,6,7)
      json.getRecursively("numb").map(_.asInt) == Seq(5,6,7)

      json.pick("result" -> "language (recursively)").asStringArray == Seq("slovac","liber","sumi")
      json.pick("language (recursively)").asStringArray == Seq("slovac","liber","sumi")
      json.getRecursively("language").map(_.asString) == Seq("slovac","liber","sumi")

      // example from the scaladoc
      /////////////////////////////
      {
        val json = J.obj(
          "a" -> J.arr(5,2,J.obj("b" -> 9)),
          "b" -> 10
        )

        json.pick("a" -> 2 -> "b").asInt === 9
        json.pick("a" -> "b (recursively)").asIntArray === List(9)
      }

      true
    }

    "Validate remaining fields, where all fields are defined" in {
      val jsonText = """
          {
            "a" : "a_",
            "b" : "b_"
          }
        """

      val json = J.parse(jsonText)

      json.validateRemaining() must throwA[JsonParseException]
      json.validateRemaining("a") must throwA[JsonParseException]
      json.validateRemaining("b") must throwA[JsonParseException]
      json.validateRemaining("a","b")

      json("a")
      json.validateRemaining() must throwA[JsonParseException]
      json.validateRemaining("a") must throwA[JsonParseException]
      json.validateRemaining("b")
      json.validateRemaining("a", "b")

      json("b")
      json.validateRemaining()
      json.validateRemaining("a")
      json.validateRemaining("b")
      json.validateRemaining("a", "b")

      J.parseAndValidate(jsonText){json =>
        json("a")
      } must throwA[JsonParseException]

      J.parseAndValidate(jsonText, ignore=Set("a")){ json =>
        json("a")
      } must throwA[JsonParseException]

      J.parseAndValidate(jsonText, ignore=Set("b")){ json =>
        json("a").asString
      } must equalTo("a_")

      J.parseAndValidate(jsonText){ json =>
        Array(json("a").asString, json("b").asString)
      } must equalTo(Array("a_","b_"))
    }

    "Validate remaining fields, where some fields are optional" in {
      val jsonText = """
          {
            "a" : "a_",
            "c" : null
          }
        """

      J.parseAndValidate(jsonText){json =>
        List(
          json("a").asString,
          json("b").asOption(_.asString)
        )
      } must throwA[JsonParseException]

      J.parseAndValidate(jsonText){json =>
        List(
          json("a").asString,
          json("b").asOption(_.asString),
          json("c").asOption(_.asString)
        )
      } must equalTo(List("a_",None,None))

    }
  }
}
