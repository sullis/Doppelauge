
J
===

* Similar to Play framework's 'Json' object, but quicker to use for testing and parsing.
* Values can easily be converted between the "J" format (called 'JValue') and the play framework format (called 'JsValue').
* J is safer than Play framework's 'Json' object since it doesn't allow key clash when creating and merging objects.
* J guarantees to maintain object field order. Play framework's 'Json' object does not.
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
* When matching fails, the error messages contain description with full path.
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


JsonChanger
===========
Change the content of a json value in a safe and descriptive manner.
  
Firstly, the name is misleading. JsonChanger is not *changing* (i.e mutating) the values. It returns a new value.

The main function, [JsonChanger.apply](http://folk.uio.no/ksvalast/doppelauge/index.html#no.samordnaopptak.json.JsonChanger$@apply%28json_value:Any,changer:Any,path:String,allow_mismatched_types:Boolean%29:no.samordnaopptak.json.JValue) takes two arguments: **json_value** and **changer**.
The **changer** variable has similarities to the **matcher** variable used in [JsonMatcher](http://folk.uio.no/ksvalast/doppelauge/#no.samordnaopptak.json.JsonMather$), but while
JsonMatcher only returns **true** or **false**, JsonChanger returns a new Json value.
   
The most important similarity between JsonChanger and JsonMatcher is that they both do pattern matching.
When changing a json value with JsonChanger, it also pattern matches the **json_value** against the **changer**. This pattern matching
should make bugs appear earlier than they would have been othervice.
(You might argue that it would be better to first validate the **json_value** value against a schema instead, but this way you get validation for free, plus that the validation schema maintains itself automatically. Besides, it doesn't make sense to create a schema for smaller json structures used internally, for instance.)
   
The pattern matcher in JsonChanger checks that:

  1. A json value doesn't change type (unless we tell it to)
  2. We don't add or remove fields to objects (unless we tell it to)
  3. We don't add or remove values to arrays (unless we tell it to)

When matching fails, the error messages contain description with full path.

There are several custom changers such as *___identity*, *Replace*, *Func*, *Map*, *MapChanger*, etc. See [scala doc](http://folk.uio.no/ksvalast/doppelauge/#no.samordnaopptak.json.JsonChanger$) for examples.
Custom changers can be also be created from outside JsonChanger by implementing the *Changer* trait.
   
ScalaDoc: [JsonChanger](http://folk.uio.no/ksvalast/doppelauge/#no.samordnaopptak.json.JsonChanger$)


```scala
import org.specs2.mutable._

import no.samordnaopptak.json.J
import no.samordnaopptak.json.JsonChanger
import no.samordnaopptak.json.JsonChanger._

object {
  val json = J.obj(
    "aaa" -> 50,
    "b" -> 1
  )

  // Change the value of "aaa" to 60 by using JsonChanger:
  JsonChanger(
    json,
    J.obj(
      "aaa" -> 60,
      "b" -> ___identity
    )
  )

  // Change the value of "aaa" to 60 manually:

  json - "aaa" ++ J.obj(
     "aaa" -> 60
  )
}
```
