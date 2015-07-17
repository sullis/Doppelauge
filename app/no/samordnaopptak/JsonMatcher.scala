package no.samordnaopptak.json

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


  /** This class does something Custom. */
  class Custom(val func: JValue => Boolean, val name: String = "") extends JsString("Custom: "+name) with JValue {
    override def pp() = "Custom: "+name
    override def asJsValue = JsNull
  }

  def Custom(func: JValue => Boolean, name: String = "") = new Custom(func, name)


  class RegExp(val pattern: String) extends JsString("RegExp: "+pattern) with JValue {
    val regexp = new scala.util.matching.Regex(pattern)

    def check(string: String): Boolean =
      regexp.findFirstIn(string) != None

    override def asJsValue = JsNull
  }
  def RegExp(pattern: String) = new RegExp(pattern)


  class Or(anyMatchers: Any*) extends JsString("Or: "+anyMatchers.map(_.toString).mkString(",")) with JValue{
    val matchers = anyMatchers.map(J(_))
    override def asJsValue = JsNull
  }
  def Or(anyMatchers: Any*) = new Or(anyMatchers :_*)


  class And(anyMatchers: Any*) extends JsString("And: "+anyMatchers.map(_.toString).mkString(",")) with JValue{
    val matchers = anyMatchers.map(J(_))
    override def asJsValue = JsNull
  }
  def And(anyMatchers: Any*) = new And(anyMatchers :_*)



  class Maybe(anyValue: Any) extends JsString("Maybe: "+anyValue.toString) with JValue {
    val matcher = Or(
      JsNull,
      anyValue
    )
    override def asJsValue = JsNull
  }
  def Maybe(anyValue: Any) = new Maybe(anyValue)


  val ___allowOtherValues: JsString = JsString(___allowOtherJsonsKey)
  val ___ignoreOrder: JsString = JsString(___ignoreOrderJsonKey)
  val ___allowOtherFields: (String,Json.JsValueWrapper) = ___allowOtherJsonsKey -> Json.toJsFieldJsValueWrapper(___allowOtherJsonsKey)


  private def matchJsonFailed(message: String, throwException: Boolean, path: String): Boolean =
    if (throwException)
      throw new JsonMatcherException(message+"\n\npath: "+path+"\n\n *** Set JsonMatcher.verbose (or JsonMatcher.very_verbose) to true to get more details. ***\n\n", path)
    else
      false

  private def getWantedNumArrayElements(matcher: JArray): Int =
    matcher.value.dropWhile(_ != JString(___numElements)).tail.head.asInt

  private def getWantedNumObjectElements(matcher: Map[String, JValue]): Int =
    matcher(___numElements).asInt

  // Seq(1, ___numElements, 2, __ignoreOrder, 3) -> Seq(1,3)
  private def getValuesWithoutNumsAndIgnores(values: Seq[JValue]): Seq[JValue] =
    if (values.isEmpty)
      values
    else if (values.head.asJsValue==JsString(___numElements))
      values.tail.tail
    else if (values.head.asJsValue==___ignoreOrder)
      getValuesWithoutNumsAndIgnores(values.tail)
    else
      Seq(values.head) ++ getValuesWithoutNumsAndIgnores(values.tail)

  private def matchOrderedJsonArrays(json: JArray, allowOthers: Boolean, matchers: Seq[JValue], pos: Int, values: Seq[JValue], throwException: Boolean, path: String): Boolean = {
    if (matchers.isEmpty)
      true

    else if (matchers.head.asJsValue == ___allowOtherValues)
      matchOrderedJsonArrays(json, allowOthers, matchers.tail, pos+1, values, throwException, path)

    else if (values.isEmpty)
      matchJsonFailed(s"${matchers.head.pp()} is not present in the array ${json.pp()}", throwException, path)

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
        matchJsonFailed(s"${values.head.pp()} in the array ${json.pp()} isn't ${matchers.head.pp()}", throwException, path)
      } else
        matchOrderedJsonArrays(json, allowOthers, matchers.tail, pos+1, values.tail, throwException, path)
    }
  }

  private def matchJsonArrays(matcher: JArray, json: JArray, throwException: Boolean, ignoreArrayOrder: Boolean, path: String): Boolean = {
    val hasAllowOthers = matcher.value.contains(J(___allowOtherValues))
    val hasIgnoreOrder = matcher.value.contains(J(___ignoreOrder)) || ignoreArrayOrder
    val hasNumElements = matcher.value.contains(J(___numElements))

    if (hasAllowOthers && hasNumElements)
      throw new Exception("Matcher contains both ___allowOthers and ___numElements")

    val cleanMatcher = getValuesWithoutNumsAndIgnores(matcher.value)
    val cleanJson = getValuesWithoutNumsAndIgnores(json.value)

    if (hasNumElements && getWantedNumArrayElements(matcher)!=cleanJson.size)
      matchJsonFailed(s"${json.pp()} contains wrong number of elements. Should contain ${getWantedNumArrayElements(matcher)}.", throwException, path)

    else if (hasAllowOthers==false && hasNumElements==false && cleanMatcher.size>cleanJson.size)
      matchJsonFailed(s"${json.pp()} contains less fields than ${matcher.pp()}.", throwException, path)

    else if (hasAllowOthers==false && hasNumElements==false && cleanMatcher.size<cleanJson.size)
      matchJsonFailed(s"${json.pp()} contains more fields than ${matcher.pp()} (Diff: ${cleanMatcher.size}<${cleanJson.size}).\n Maybe you forgot to add an ___allowOtherValues value to the matcher.", throwException, path)

    else if (hasIgnoreOrder)
      cleanMatcher.forall( (matchValue: JValue) =>
        if (matchValue.asJsValue == ___allowOtherValues)
          true
        else if (cleanJson.exists(matchJson(matchValue, _, false, ignoreArrayOrder, path))==false) {
          if (verbose && json.value.size==1)
            matchJson(cleanJson.head, matchValue, true, ignoreArrayOrder, path)
          matchJsonFailed(s"""${json.pp()} doesn't contain the value "${matchValue.pp()}"""", throwException, path)
        }else
          true
      )
    else
      matchOrderedJsonArrays(json, hasAllowOthers, cleanMatcher, 0, cleanJson, throwException, path)
  }

  private def matchJsonObjects(matcher: JValue, jsonWithoutMaybes: JValue, throwException: Boolean, ignoreArrayOrder: Boolean, path: String): Boolean = {
    val maybeKeys = matcher.asMap.filter{
      case (_, value: Maybe) => true
      case _                 => false
    }.keys
    val unusedMaybeKeys = maybeKeys.filter( key => !jsonWithoutMaybes.keys.contains(key))
    val maybes = JObject(
      unusedMaybeKeys.map(key =>
        key -> JNull
      ).toMap
    )

    val json = jsonWithoutMaybes ++ maybes

    val hasAllowOthers = matcher.keys.contains(___allowOtherJsonsKey)
    val hasNumElements = matcher.keys.contains(___numElements)
    val matcherSize = if (hasNumElements) matcher.size-1 else matcher.size

    if (hasAllowOthers && hasNumElements)
      throw new Exception("Matcher contains both ___allowOthers and ___numElements")

    if (hasNumElements && getWantedNumObjectElements(matcher.asMap) != json.size)
      matchJsonFailed(s"${matcher.pp()} contains wrong number of elements. Should contain ${getWantedNumObjectElements(matcher.asMap)}, contains ${json.size}.", throwException, path)

    else if (hasAllowOthers==false && hasNumElements==false && matcher.size>json.size) {
      val keys1 = json.keys
      val keys2 = matcher.keys
      val diff = keys2 -- keys1

      matchJsonFailed(s"In ${matcher.pp()}, the following fields are added: ${diff.mkString(", ")}\nMaybe you forgot to add an ___allowOtherFields value to the matcher.", throwException, path)

    } else if (hasAllowOthers==false && hasNumElements==false && matcher.size<json.size) {

      val keys1 = json.keys
      val keys2 = matcher.keys
      val diff = keys1 -- keys2

      matchJsonFailed(s"In ${matcher.pp()}, the following fields are missing: ${diff.mkString(", ")}", throwException, path)

    } else
      matcher.asMap.forall(_ match{
        case (key: String, value: JValue) =>
          if (key == ___numElements)
            true
          else if (key == ___allowOtherJsonsKey)
            true
          else if (json.keys.contains(key)==false)
            matchJsonFailed(s"""${json.pp()} doesn't contain the key "$key", which is defined in the matcher: ${matcher.pp()}""", throwException, path)
          else
            matchJson(value, json(key), throwException, ignoreArrayOrder, if (path=="") key else path+"."+key)
      })
  }

  private def matchJsonObjects(matcher: JsObject, jsonWithoutMaybes: JsObject, throwException: Boolean, ignoreArrayOrder: Boolean, path: String): Boolean =
    matchJsonObjects(J(matcher), J(jsonWithoutMaybes), throwException, ignoreArrayOrder, path)

  private def getStackTraceString(t: Throwable) = {
    val sw = new java.io.StringWriter()
    t.printStackTrace(new java.io.PrintWriter(sw))
    sw.toString()
  }

  private def matchCustom(custom: Custom, json: JValue, throwException: Boolean, path: String): Boolean = {
    val result = try{
      custom.func(json)
    } catch {
      case e: Throwable => {
        matchJsonFailed(s""""The custom matcher $custom threw an exception: "${getStackTraceString(e)}" when called with "${json.pp()}".""", throwException, path)
      }
    }

    if (result==true)
      true
    else
      matchJsonFailed(s""""The custom matcher function $custom returned false when called with "${json.pp()}".""", throwException, path)
  }

  private def matchOr(or: Or, json: JValue, throwException: Boolean, ignoreArrayOrder: Boolean, path: String): Boolean = {
    def inner(matchers: Seq[JValue]): Boolean =
      if (matchers.isEmpty && throwException)
        matchJsonFailed(s"""Doesn't match:\n${or.matchers.map(_.pp()).mkString("Or(", ", ", ")")}\n VS. ${json.pp()}""", throwException, path)
      else if (matchers.isEmpty)
        false
      else if (matchJson(matchers.head, json, false, ignoreArrayOrder, path))
        true
      else
        inner(matchers.tail)

    inner(or.matchers)
  }

  private def matchAnd(and: And, json: JValue, throwException: Boolean, ignoreArrayOrder: Boolean, path: String): Boolean = {
    def inner(matchers: Seq[JValue]): Boolean =
      if (matchers.isEmpty)
        true
      else if (matchJson(matchers.head, json, false, ignoreArrayOrder, path))
        inner(matchers.tail)
      else if (throwException)
        matchJsonFailed(s"""Doesn't match:\n${and.matchers.map(_.pp()).mkString("And(", ", ", ")")}\n VS. ${json.pp()}""", throwException, path)
      else
        false

    inner(and.matchers)
  }

  private def matchJson(matcher: JValue, json: JValue, throwException: Boolean, ignoreArrayOrder: Boolean, path: String): Boolean = {

    if (verbose && matcher.isString && json.isString)
      println("matching "+matcher.asString+", vs. "+json.asString)
    else if (very_verbose)
      println("matching "+matcher.pp()+", vs. "+json.pp())

    val success = matcher match {
      case (m: Or      ) => matchOr(m, json, throwException, ignoreArrayOrder, path)
      case (m: And     ) => matchAnd(m, json, throwException, ignoreArrayOrder, path)
      case (m: Maybe   ) => matchJson(m.matcher, json,  throwException, ignoreArrayOrder, path)
      case (c: Custom  ) => matchCustom(c, json, throwException, path)
      case (r: RegExp  ) if json.isString => if (r.check(json.asString)) true else matchJsonFailed(s""""${json.asString}" doesn't match the regexp "${r.pattern}".""", throwException, path)
      case (r: RegExp  ) => matchJsonFailed(s"""RegExp matcher expected a string. Not-string value: "${json.pp()}"""", throwException, path)
      case _ => (matcher.asJsValue, json) match {
        case (`___anyString`,  j: JString) => true
        case (`___anyNumber`,  j: JNumber) => true
        case (`___anyObject`,  j: JObject) => true
        case (`___anyArray`,   j: JArray) => true
        case (`___anyBoolean`, j: JBoolean) => true
        case (m: JsObject,     j: JObject) => matchJsonObjects(matcher.asInstanceOf[JObject],j,throwException,ignoreArrayOrder,path)
        case (m: JsArray,      j: JArray)  => matchJsonArrays(matcher.asInstanceOf[JArray],j,throwException,ignoreArrayOrder,path)
        case (m, j) => if (m==j.asJsValue) true else matchJsonFailed(s"Doesn't match. Expected ${matcher.pp()}. Got ${json.pp()}", throwException, path)
      }
    }

    if (verbose && success)
      println("...success")
    if (verbose && !success)
      println("...failed")

    success
  }

  private def matchJson(any_matcher: Any, any_json: Any, throwException: Boolean, ignoreArrayOrder: Boolean, path: String): Boolean = {
    val matcher = J(any_matcher)
    val json = J(any_json)
    matchJson(matcher, json, throwException, ignoreArrayOrder, path)
  }


  /** This is a brief description of what's being documented.
  *
  * This is further documentation of what we're documenting.  It should
  * provide more details as to how this works and what it does. 
  */
  def matchJson(matcher: Any, json: Any, throwException: Boolean = true, ignoreArrayOrder: Boolean = false): Boolean =
    matchJson(matcher, json, throwException, ignoreArrayOrder, "")


}
