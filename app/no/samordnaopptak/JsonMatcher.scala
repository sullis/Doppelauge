package no.samordnaopptak.apidoc

import play.api.libs.json._


/** This is the ScalaDoc for the package JsonMatcher. */
object JsonMatcher{
  var very_verbose = false
  var verbose = very_verbose || false

  class JsonMatcherException(message: String, val path: String) extends Exception(message)

  private val ___allowOtherJsonsKey = "________allowothers________"
  private val ___ignoreOrderJsonKey = "________ignoreOrder________"
  private val ___maybeFieldPrefix = "________maybe_______"

  val ___numElements = "________numFields________"

  /** This value does something stringy.
    * {{{
    *   JsonMatcher.matchJson(
    *     JsonMatcher.___anyString,
    *     JsString("gakk")
    *   )
    * }}}
    */
  val ___anyString  = JsString("________anyString________")

  val ___anyNumber  = JsString("________anyNumber________")
  val ___anyObject  = JsString("________anyObject________")
  val ___anyArray   = JsString("________anyArray_________")
  val ___anyBoolean = JsString("________anyBoolean_______")


  private def wrapperToJsValue(wrapper: Json.JsValueWrapper) =
    Json.arr(wrapper).value(0)

  /** This class does something Custom. */
  case class Custom(func: JsValue => Boolean, name: String = "") extends JsUndefined("custom") {
    override def toString = if (name=="") func.toString else name
  }

  case class RegExp(pattern: String) extends JsUndefined("regexp") {
    val regexp = new scala.util.matching.Regex(pattern)

    def apply(jsString: JsString): Boolean = {
      regexp.findFirstIn(jsString.value) != None
    }
  }

  case class Or(wrapperMatchers: Json.JsValueWrapper*) extends JsUndefined("or") {
    val matchers = wrapperMatchers.map(wrapperToJsValue)
  }

  case class And(wrapperMatchers: Json.JsValueWrapper*) extends JsUndefined("and") {
    val matchers = wrapperMatchers.map(wrapperToJsValue)
  }

  case class Maybe(wrapperValue: Json.JsValueWrapper) extends JsUndefined("maybe") {
    val matcher = Or(
      JsNull,
      wrapperToJsValue(wrapperValue)
    )
  }

  val ___allowOtherValues: JsString = JsString(___allowOtherJsonsKey)
  val ___ignoreOrder: JsString = JsString(___ignoreOrderJsonKey)
  val ___allowOtherFields: (String,Json.JsValueWrapper) = ___allowOtherJsonsKey -> Json.toJsFieldJsValueWrapper(___allowOtherJsonsKey)


  private def pp(json: JsValue): String =
    s"\n${Json.prettyPrint(json)}\n"

  private def matchJsonFailed(message: String, throwException: Boolean, path: String): Boolean =
    if (throwException)
      throw new JsonMatcherException(message+"\n\npath: "+path+"\n\n *** Set JsonMatcher.verbose (or JsonMatcher.very_verbose) to true to get more details. ***\n\n", path)
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
    else if (values.head==___ignoreOrder)
      getValuesWithoutNumsAndIgnores(values.tail)
    else
      Seq(values.head) ++ getValuesWithoutNumsAndIgnores(values.tail)

  private def matchOrderedJsonArrays(json: JsArray, allowOthers: Boolean, matchers: Seq[JsValue], pos: Int, values: Seq[JsValue], throwException: Boolean, path: String): Boolean = {
    if (matchers.isEmpty)
      true

    else if (matchers.head == ___allowOtherValues)
      matchOrderedJsonArrays(json, allowOthers, matchers.tail, pos+1, values, throwException, path)

    else if (values.isEmpty)
      matchJsonFailed(s"${pp(matchers.head)} is not present in the array ${pp(json)}", throwException, path)

    else if (allowOthers) {
      val matches = matchJson(matchers.head, values.head, false, false, path+"("+pos+")")
      if (matches==false)
        matchOrderedJsonArrays(json, allowOthers, matchers, pos, values.tail, throwException, path)
      else
        matchOrderedJsonArrays(json, allowOthers, matchers.tail, pos+1, values.tail, throwException, path)

    } else {
      val matches = matchJson(matchers.head, values.head, throwException, false, path+"("+pos+")")
      if (matches==false) {
        if (verbose && values.size==1)
          matchJson(matchers.head, values.head, true, false, path)
        matchJsonFailed(s"${pp(values.head)} in the array ${pp(json)} isn't ${pp(matchers.head)}", throwException, path)
      } else
        matchOrderedJsonArrays(json, allowOthers, matchers.tail, pos+1, values.tail, throwException, path)
    }
  }

  private def matchJsonArrays(matcher: JsArray, json: JsArray, throwException: Boolean, ignoreArrayOrder: Boolean, path: String): Boolean = {
    val hasAllowOthers = matcher.value.contains(___allowOtherValues)
    val hasIgnoreOrder = matcher.value.contains(___ignoreOrder) || ignoreArrayOrder
    val hasNumElements = matcher.value.contains(JsString(___numElements))

    if (hasAllowOthers && hasNumElements)
      throw new Exception("Matcher contains both ___allowOthers and ___numElements")

    val cleanMatcher = getValuesWithoutNumsAndIgnores(matcher.value)
    val cleanJson = getValuesWithoutNumsAndIgnores(json.value)

    if (hasNumElements && getWantedNumArrayElements(matcher)!=cleanJson.size)
      matchJsonFailed(s"${pp(json)} contains wrong number of elements. Should contain ${getWantedNumArrayElements(matcher)}.", throwException, path)

    else if (hasAllowOthers==false && hasNumElements==false && cleanMatcher.size>cleanJson.size)
      matchJsonFailed(s"${pp(json)} contains less fields than ${pp(matcher)}.", throwException, path)

    else if (hasAllowOthers==false && hasNumElements==false && cleanMatcher.size<cleanJson.size)
      matchJsonFailed(s"${pp(json)} contains more fields than ${pp(matcher)} (Diff: ${cleanMatcher.size}<${cleanJson.size}).\n Maybe you forgot to add an ___allowOtherValues value to the matcher.", throwException, path)

    else if (hasIgnoreOrder)
      cleanMatcher.forall( (matchValue: JsValue) =>
        if (matchValue == ___allowOtherValues)
          true
        else if (cleanJson.exists(matchJson(matchValue, _, false, ignoreArrayOrder, path))==false) {
          if (verbose && json.value.size==1)
            matchJson(cleanJson.head, matchValue, true, ignoreArrayOrder, path)
          matchJsonFailed(s"""${pp(json)} doesn't contain the value "${pp(matchValue)}"""", throwException, path)
        }else
          true
      )
    else
      matchOrderedJsonArrays(json, hasAllowOthers, cleanMatcher, 0, cleanJson, throwException, path)
  }

  private def matchJsonObjects(matcher: JsObject, jsonWithoutMaybes: JsObject, throwException: Boolean, ignoreArrayOrder: Boolean, path: String): Boolean = {
    val maybeKeys = matcher.value.filter{
      case (_, value: Maybe) => true
      case _                 => false
    }.keys

    val unusedMaybeKeys = maybeKeys.filter( key => !jsonWithoutMaybes.keys.contains(key))

    val maybes = JsObject(
      unusedMaybeKeys.map(key =>
        key -> JsNull
      ).toSeq
    )

    val json = jsonWithoutMaybes ++ maybes

    val hasAllowOthers = matcher.keys.contains(___allowOtherJsonsKey)
    val hasNumElements = matcher.keys.contains(___numElements)
    val matcherSize = if (hasNumElements) matcher.value.size-1 else matcher.value.size

    if (hasAllowOthers && hasNumElements)
      throw new Exception("Matcher contains both ___allowOthers and ___numElements")

    if (hasNumElements && getWantedNumObjectElements(matcher) != json.value.size)
      matchJsonFailed(s"${pp(matcher)} contains wrong number of elements. Should contain ${getWantedNumObjectElements(matcher)}, contains ${json.value.size}.", throwException, path)

    else if (hasAllowOthers==false && hasNumElements==false && matcher.value.size>json.value.size) {
      val keys1 = json.keys
      val keys2 = matcher.keys
      val diff = keys2 -- keys1

      matchJsonFailed(s"In ${pp(matcher)}, the following fields are added: ${diff.mkString(", ")}\nMaybe you forgot to add an ___allowOtherFields value to the matcher.", throwException, path)

    } else if (hasAllowOthers==false && hasNumElements==false && matcher.value.size<json.value.size) {
      val keys1 = json.keys
      val keys2 = matcher.keys
      val diff = keys1 -- keys2

      matchJsonFailed(s"In ${pp(matcher)}, the following fields are missing: ${diff.mkString(", ")}", throwException, path)

    } else
      matcher.fields.forall(_ match{
        case (key: String, value: JsValue) =>
          if (key == ___numElements)
            true
          else if (key == ___allowOtherJsonsKey)
            true
          else if (json.keys.contains(key)==false)
            matchJsonFailed(s"""${pp(json)} doesn't contain the key "$key", which is defined in the matcher: ${pp(matcher)}""", throwException, path)
          else
            matchJson(value, json.\(key), throwException, ignoreArrayOrder, if (path=="") key else path+"."+key)
      })
  }

  private def getStackTraceString(t: Throwable) = {
    val sw = new java.io.StringWriter()
    t.printStackTrace(new java.io.PrintWriter(sw))
    sw.toString()
  }

  private def matchCustom(custom: Custom, json: JsValue, throwException: Boolean, path: String): Boolean = {
    val result = try{
      custom.func(json)
    } catch {
      case e: Throwable => {
        matchJsonFailed(s""""The custom matcher $custom threw an exception: "${getStackTraceString(e)}" when called with "${pp(json)}".""", throwException, path)
      }
    }

    if (result==true)
      true
    else
      matchJsonFailed(s""""The custom matcher function $custom returned false when called with "${pp(json)}".""", throwException, path)
  }

  private def matchOr(or: Or, json: JsValue, throwException: Boolean, ignoreArrayOrder: Boolean, path: String): Boolean = {
    def inner(matchers: Seq[JsValue]): Boolean =
      if (matchers.isEmpty && throwException)
        matchJsonFailed(s"""Doesn't match:\n${or.matchers.map(Json.prettyPrint(_)).mkString("Or(", ", ", ")")}\n VS. ${pp(json)}""", throwException, path)
      else if (matchers.isEmpty)
        false
      else if (matchJson(matchers.head, json, false, ignoreArrayOrder, path))
        true
      else
        inner(matchers.tail)

    inner(or.matchers)
  }

  private def matchAnd(and: And, json: JsValue, throwException: Boolean, ignoreArrayOrder: Boolean, path: String): Boolean = {
    def inner(matchers: Seq[JsValue]): Boolean =
      if (matchers.isEmpty)
        true
      else if (matchJson(matchers.head, json, false, ignoreArrayOrder, path))
        inner(matchers.tail)
      else if (throwException)
        matchJsonFailed(s"""Doesn't match:\n${and.matchers.map(Json.prettyPrint(_)).mkString("And(", ", ", ")")}\n VS. ${pp(json)}""", throwException, path)
      else
        false

    inner(and.matchers)
  }

  private def matchJson(matcherWrapper: Json.JsValueWrapper, jsonWrapper: Json.JsValueWrapper, throwException: Boolean, ignoreArrayOrder: Boolean, path: String): Boolean = {
    val matcher = wrapperToJsValue(matcherWrapper)
    val json = wrapperToJsValue(jsonWrapper)

    if (verbose && matcher.isInstanceOf[JsString] && json.isInstanceOf[JsString])
      println("matching "+matcher+", vs. "+json)
    else if (very_verbose)
      println("matching "+pp(matcher)+", vs. "+pp(json))

    val success = (matcher, json) match{
      case (`___anyString`,  j: JsString) => true
      case (`___anyNumber`,  j: JsNumber) => true
      case (`___anyObject`,  j: JsObject) => true
      case (`___anyArray`,   j: JsArray) => true
      case (`___anyBoolean`, j: JsBoolean) => true
      case (m: Or,           j: JsValue)   => matchOr(m, j, throwException, ignoreArrayOrder, path)
      case (m: And,          j: JsValue)   => matchAnd(m, j, throwException, ignoreArrayOrder, path)
      case (m: Maybe,        j: JsValue)   => matchJson(m.matcher, j,  throwException, ignoreArrayOrder, path)
      case (c: Custom,       j: JsValue) => matchCustom(c, j, throwException, path)
      case (r: RegExp,     j: JsString) => if (r(j)) true else matchJsonFailed(s""""${j.value}" doesn't match the regexp "${r.pattern}".""", throwException, path)
      case (r: RegExp,     j: JsValue) => matchJsonFailed(s"""RegExp matcher expected a string. Not-string value: "$j"""", throwException, path)
      case (m: JsObject, j: JsObject) => matchJsonObjects(m,j,throwException,ignoreArrayOrder,path)
      case (m: JsArray,  j: JsArray)  => matchJsonArrays(m,j,throwException,ignoreArrayOrder,path)
      case (m: JsValue,  j: JsValue)  => if (m==j) true else matchJsonFailed(s"Doesn't match: ${pp(matcher)} VS. ${pp(json)}", throwException, path)
    }

    if (verbose && success)
      println("...success")
    if (verbose && !success)
      println("...failed")

    success
  }

  /** This is a brief description of what's being documented.
  *
  * This is further documentation of what we're documenting.  It should
  * provide more details as to how this works and what it does. 
  */
  def matchJson(matcher: Json.JsValueWrapper, json: Json.JsValueWrapper, throwException: Boolean = true, ignoreArrayOrder: Boolean = false): Boolean =
    matchJson(matcher, json, throwException, ignoreArrayOrder, "")
}
