package test

import org.specs2.mutable._

import no.samordnaopptak.json._

import play.api.libs.{json => pjson}
import play.api.libs.json.{Json => PJson} // i.e. Play Json


class JsonUtilSpec extends Specification {

  "JsonUtil" should {

    "JsonUtil.obj" in {

      JsonUtil.obj().asJsObject === PJson.obj()

      JsonUtil.obj(
        "a" -> "b"
      ).asJsObject === PJson.obj(
        "a" -> "b"
      )

      JsonUtil.obj(
        "a" -> 1,
        "b" -> 1.2,
        "c" -> 5134134L
      ).asJsObject === PJson.obj(
        "a" -> 1,
        "b" -> 1.2,
        "c" -> 5134134L
      )

      JsonUtil.obj(
        "a" -> true
      ).asJsObject === PJson.obj(
        "a" -> true
      )

      JsonUtil.obj(
        "a" -> PJson.arr(0)
      ).asJsObject === PJson.obj(
        "a" -> PJson.arr(0)
      )

      JsonUtil.obj(
        "a" -> JsonUtil.obj("c" -> "d")
      ).asJsObject === PJson.obj(
        "a" -> PJson.obj("c" -> "d")
      )

      true
    }

    "JsonUtil.arr" in {

      JsonUtil.arr().asJsValue === PJson.arr()

      JsonUtil.arr(
        1, "2", false
      ).asJsValue === PJson.arr(
        1, "2", false
      )

      JsonUtil.arr(
        1,
        "2",
        JsonUtil.obj(
          "a" -> JsonUtil.arr(50)
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
      JsonUtil.parse("{]") must throwA[JsonParseException]
      JsonUtil.parse(""" { "gakk": 50 "o:" } """) must throwA[JsonParseException]
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

    val json = JsonUtil.parse(jsonString)

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

    "JsonUtil from map" in {
      JsonUtil(
        Map(
          "a" -> 50,
          "b" -> 60
        )
      ).asJsObject === play.api.libs.json.Json.obj(
        "a" -> 50,
        "b" -> 60
      )
    }

    "Json.size, Json.keys and Json.++" in {
      val o = JsonUtil(
        play.api.libs.json.Json.obj(
          "a" -> 50,
          "b" -> 60
        )
      )
      val a = JsonUtil(
        play.api.libs.json.Json.arr("a","b","c")
      )

      o.size must equalTo(2)
      a.size must equalTo(3)
      o.keys must equalTo(Set("a","b"))
      a.keys must throwA[JsonException]

      (json ++ json) must equalTo(json)
      (o ++ o) must equalTo(o)

      (json ++ o).size must equalTo(json.size + o.size)

      (a ++ o) must throwA[JsonException]
    }

    "parseIt" in {

      val jsonText = """
          {
            "a" : "a_",
            "b" : "b_"
          }
        """

      JsonUtil.parseIt(jsonText) { json =>
        true
      } === true

      JsonUtil.parseIt(jsonText) { json =>
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

      val json = JsonUtil.parse(jsonText)

      json("a").isDefined === true
      json("b").isDefined === false
      json("c").isDefined === false

      json.hasKey("a") === true
      json.hasKey("b") === true
      json.hasKey("c") === false
    }

    "Validate remaining fields, where all fields are defined" in {
      val jsonText = """
          {
            "a" : "a_",
            "b" : "b_"
          }
        """

      val json = JsonUtil.parse(jsonText)

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

      JsonUtil.parseAndValidate(jsonText){json =>
        json("a")
      } must throwA[JsonParseException]

      JsonUtil.parseAndValidate(jsonText, ignore=Set("a")){ json =>
        json("a")
      } must throwA[JsonParseException]

      JsonUtil.parseAndValidate(jsonText, ignore=Set("b")){ json =>
        json("a").asString
      } must equalTo("a_")

      JsonUtil.parseAndValidate(jsonText){ json =>
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

      JsonUtil.parseAndValidate(jsonText){json =>
        List(
          json("a").asString,
          json("b").asOption(_.asString)
        )
      } must throwA[JsonParseException]

      JsonUtil.parseAndValidate(jsonText){json =>
        List(
          json("a").asString,
          json("b").asOption(_.asString),
          json("c").asOption(_.asString)
        )
      } must equalTo(List("a_",None,None))

    }

  }
}
