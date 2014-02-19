package no.samordnaopptak.apidoc

import play.api.libs.json._

// This file and test/lib/JsonMatcherSpec.scala are present in both sokerportal, tk and sdf. If changing any of those two files, all three repositories should be updated.


object JsonMatcher{
  private val very_verbose = false
  private val verbose = very_verbose || false

  class JsonMatcherException(message: String) extends Exception(message)

  private val ___allowOtherJsonsKey = "________allowothers________"
  private val ___ignoreOrderJsonKey = "________ignoreOrder________"
  val ___numElements = "________numFields________"
  val ___anyString = JsString("________numFields________")

  val ___allowOtherValues: JsString = JsString(___allowOtherJsonsKey)
  val ___ignoreOrder: JsString = JsString(___ignoreOrderJsonKey)
  val ___allowOtherFields: (String,Json.JsValueWrapper) = ___allowOtherJsonsKey -> Json.toJsFieldJsValueWrapper(___allowOtherJsonsKey)

  private def pp(json: JsValue): String =
    s"\n${Json.prettyPrint(json)}\n"

  private def matchJsonFailed(message: String, throwException: Boolean): Boolean =
    if (throwException)
      throw new JsonMatcherException(message+"\n\n *** Set JsonMatcher.verbose to true to get more details. ***\n\n")
    else
      false

  private def getWantedNumArrayElements(matcher: JsArray): Int =
    matcher.value.dropWhile(_ != JsString(___numElements)).tail.head.asInstanceOf[JsNumber].value.toInt

  private def getWantedNumObjectElements(matcher: JsObject): Int =
    matcher.value(___numElements).asInstanceOf[JsNumber].value.toInt

  // Seq(1, ___numElements, 2, __ignoreOrder, 3) -> Seq(1,3)
  private def getValuesWithoutNumsAndIgnores(values: Seq[JsValue]): Seq[JsValue] =
    if (values.isEmpty)
      values
    else if (values.head==JsString(___numElements))
      values.tail.tail
    else if (values.head==JsString(___ignoreOrderJsonKey))
      getValuesWithoutNumsAndIgnores(values.tail)
    else
      Seq(values.head) ++ getValuesWithoutNumsAndIgnores(values.tail)

  private def matchOrderedJsonArrays(json: JsArray, allowOthers: Boolean, matchers: Seq[JsValue], values: Seq[JsValue], throwException: Boolean): Boolean = {
    if (matchers.isEmpty)
      true
    else if (matchers.head == ___allowOtherValues)
      matchOrderedJsonArrays(json, allowOthers, matchers.tail, values, throwException)
    else if (values.isEmpty)
      matchJsonFailed(s"${pp(matchers.head)} is not present in the array ${pp(json)}", throwException)
    else {
      val matches = matchJson(matchers.head, values.head, false)
      if (matches==false && allowOthers==true)
        matchOrderedJsonArrays(json, allowOthers, matchers, values.tail, throwException)
      else if (matches==false) {
        if (verbose && values.size==1)
          matchJson(matchers.head, values.head, true)
        matchJsonFailed(s"${pp(values.head)} in the array ${pp(json)} isn't ${pp(matchers.head)}", throwException)
      } else
        matchOrderedJsonArrays(json, allowOthers, matchers.tail, values.tail, throwException)
    }
  }

  private def matchJsonArrays(matcher: JsArray, json: JsArray, throwException: Boolean = true): Boolean = {
    val hasAllowOthers = matcher.value.contains(___allowOtherValues)
    val hasIgnoreOrder = matcher.value.contains(___ignoreOrder)
    val hasNumElements = matcher.value.contains(JsString(___numElements))

    if (hasAllowOthers && hasNumElements)
      throw new Exception("Matcher contains both ___allowOthers and ___numElements")

    val cleanMatcher = getValuesWithoutNumsAndIgnores(matcher.value)

    if (hasNumElements && getWantedNumArrayElements(matcher)!=json.value.size)
      matchJsonFailed(s"${pp(json)} contains wrong number of elements. Should contain ${getWantedNumArrayElements(matcher)}.", throwException)

    else if (hasAllowOthers==false && hasNumElements==false && cleanMatcher.size>json.value.size)
      matchJsonFailed(s"${pp(json)} contains less fields than ${pp(matcher)}.", throwException)

    else if (hasAllowOthers==false && hasNumElements==false && cleanMatcher.size<json.value.size)
      matchJsonFailed(s"${pp(json)} contains more fields than ${pp(matcher)}.\n Maybe you forgot to add an ___allowOtherValues value to the matcher.", throwException)

    else if (hasIgnoreOrder)
      cleanMatcher.forall( (matchValue: JsValue) =>
        if (matchValue == ___allowOtherValues)
          true
        else if (json.value.exists(matchJson(matchValue, _, false))==false) {
          if (verbose && json.value.size==1)
            matchJson(json.value.head, matchValue, true)
          matchJsonFailed(s"${pp(json)} doesn't contain the value ${pp(matchValue)}", throwException)
        }else
          true
      )
    else
      matchOrderedJsonArrays(json, hasAllowOthers, cleanMatcher, json.value, throwException)
  }

  private def matchJsonObjects(matcher: JsObject, json: JsObject, throwException: Boolean = true): Boolean = {
    val hasAllowOthers = matcher.keys.contains(___allowOtherJsonsKey)
    val hasNumElements = matcher.keys.contains(___numElements)
    val matcherSize = if (hasNumElements) matcher.value.size-1 else matcher.value.size

    if (hasAllowOthers && hasNumElements)
      throw new Exception("Matcher contains both ___allowOthers and ___numElements")

    if (hasNumElements && getWantedNumObjectElements(matcher)!=json.value.size)
      matchJsonFailed(s"${pp(json)} contains wrong number of elements. Should contain ${getWantedNumObjectElements(matcher)}, contains ${json.value.size}.", throwException)

    else if (hasAllowOthers==false && hasNumElements==false && matcher.value.size>json.value.size)
      matchJsonFailed(s"${pp(json)} contains less fields than ${pp(matcher)}.", throwException)

    else if (hasAllowOthers==false && hasNumElements==false && matcher.value.size<json.value.size)
      matchJsonFailed(s"${pp(json)} contains more fields than ${pp(matcher)}. Maybe you forgot to add an ___allowOtherFields value to the matcher.", throwException)

    else
      matcher.fields.forall(_ match{
        case (key: String, value: JsValue) =>
          if (key == ___numElements)
            true
          else if (key == ___allowOtherJsonsKey)
            true
          else if (json.keys.contains(key)==false)
            matchJsonFailed(s"${pp(json)} doesn't contain the key $key", throwException)
          else
            matchJson(value, json.\(key), throwException)
      })
  }

  def matchJson(matcher: JsValue, json: JsValue, throwException: Boolean = true): Boolean = {
    if (verbose && matcher.isInstanceOf[JsString] && json.isInstanceOf[JsString])
      println("matching "+matcher+", vs. "+json)
    else if (very_verbose)
      println("matching "+pp(matcher)+", vs. "+pp(json))

    val success = (matcher, json) match{
      case (m: JsObject, j: JsObject) => matchJsonObjects(m,j,throwException)
      case (m: JsArray,  j: JsArray)  => matchJsonArrays(m,j,throwException)
      case (`___anyString`, j: JsString) => true
      case (m: JsValue,  j: JsValue)  => if (m==j) true else matchJsonFailed(s"Doesn't match: ${pp(matcher)} VS. ${pp(json)}", throwException)
    }

    if (verbose && success)
      println("...success")
    if (verbose && !success)
      println("...failed")

    success
  }
}
