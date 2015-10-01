
J
===

* Similar to Play framework's 'Json' object, but quicker to use for testing and parsing.
* Values can easily be converted between the "J" format (called 'JValue') and the play framework format (called 'JsValue').
* Scaladoc: [J](http://folk.uio.no/ksvalast/doppelauge/#no.samordnaopptak.json.J$)


```scala
val json = J.obj(
  "a" -> 1,
  "b" -> J.arr(2,3)
)

json("a").asInt === 1
json("b")(0).asInt === 2
json("b")(1).asInt === 3

val json2 = J.parse("[4,5,6]")
json2(0).asInt === 4
json2(1).asInt === 5
json2(2) === JNumber(6)
```


JsonMatcher
===========

* Patternmatcher for json values.
* Several custom matchers.
* ScalaDoc: [JsonMatcher](http://folk.uio.no/ksvalast/doppelauge/#no.samordnaopptak.json.JsonMatcher$)


```scala
package test

import org.specs2.mutable._

import no.samordnaopptak.json.J
import no.samordnaopptak.json.JsonMatcher._

class JsonMatcherSpec extends Specification {
  "Json matcher" should {
    "match object with object" in {
      matchJson(
        J.obj(
          "a" -> J.obj(
            "c" -> ___anyString
            "e" -> ___anyArray
          )
        ),
        J.obj(
          "a" -> J.obj(
            "d" -> "astring",
            "e" -> J.arr(2,3,4,5)
          )
        )
      )
    }
  }
}
```
