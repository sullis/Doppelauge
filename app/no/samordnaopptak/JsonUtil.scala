package no.samordnaopptak.apidoc


object JsonUtil {


  import play.api.libs.json._


  class JsonParseException(val message: String) extends Exception(message)

  trait Json{
    private var visitedKeys: Set[String] = Set()

    def pp(): String

    override def toString =
      "JsonUtil.Json(\n"+pp()+"\n)"

    def asJsObject: JsObject
    def asMap: scala.collection.Map[String, Json]
    def asList: List[Json]
    def asString: String
    def asBoolean: Boolean
    def asNumber: BigDecimal

    def asDouble = asNumber.toDouble
    def asInt = asNumber.toInt
    def asLong = asNumber.toLong

    def isMap: Boolean
    def isNumber: Boolean
    def isBoolean: Boolean
    def isString: Boolean

    def isDefined: Boolean
    def hasKey(key: String): Boolean // it might be undefined even if it has key. (i.e. when the value is null)

    def asOption[R](command: Json => R): Option[R] = 
      Some(command(this))

    def getOrElse[R](command: Json => R, orElseValue: R): R =
      command(this)

    def asLongList = asList.map(_.asLong)
    def asIntList = asList.map(_.asInt)
    def asDoubleList = asList.map(_.asDouble)
    def asStringList = asList.map(_.asString)
    def asBooleanList = asList.map(_.asBoolean)

    def apply(key: String) =
      asMap.get(key) match {
        case Some(json) => {
          visitedKeys = visitedKeys + key
          json
        }
        case None =>
          JsonWithoutValue(key, this)
      }

    def apply(index: Int) =
      try{
        asList(index)
      }catch{
        case e: java.lang.IndexOutOfBoundsException => throw new JsonParseException("index "+index+" not found in "+pp()+" ("+e.getMessage()+")")
      }

    def validateRemaining(ignoreKeys: Set[String]): Unit = {
      val visitedKeys = this.visitedKeys ++ ignoreKeys
      val diff = asMap.keys.toSet.diff(visitedKeys)

      if (!diff.isEmpty)
        throw new JsonParseException(s"""Unknown field(s): ${diff.mkString("\"", "\", \"", "\"")}""")
    }

    def validateRemaining(ignoreKeys: String*): Unit =
      validateRemaining(ignoreKeys.toSet)
  }

  case class JsonWithoutValue(key: String, parent: Json) extends Json{
    def pp() = "<not defined>"

    private def error =
      throw new JsonParseException("""Trying to access key """"+key+"""", which is not found in """+parent)

    def asJsObject = error
    def asMap = error
    def asList = error
    def asString = error
    def asNumber = error
    def asBoolean = error

    def isMap = error
    def isNumber: Boolean = error
    def isBoolean: Boolean = error
    def isString: Boolean = error

    def isDefined: Boolean = false
    def hasKey(key: String): Boolean = false

    override def asOption[R](command: Json => R): Option[R] = None
    override def getOrElse[R](command: Json => R, orElseValue: R): R =
      orElseValue
  }

  case class JsonWithValue(json: JsValue) extends Json{  
    def pp() =
      play.api.libs.json.Json.prettyPrint(json)

    private def catchCommon[R](t: String)(command: => R): R = try{
      command
    } catch {
      case e: java.lang.ClassCastException => throw new JsonParseException("'" + pp() + s"' can not be converted to $t. ("+e.getMessage()+")")
      case e: JsResultException => throw new JsonParseException("'" + pp() + s"' can not be converted to $t. ("+e.getMessage()+")")
    }

    lazy val asMap: scala.collection.Map[String, Json] = catchCommon("Map"){
      json.as[JsObject].value.map{
        case (key, `JsNull`) => key -> JsonWithoutValue(key, this)
        case (key, value) => key -> JsonWithValue(value)
      }
    }

    lazy val asList = catchCommon("List"){
      json.as[JsArray].value.toList.map(JsonWithValue(_))
    }

    def asJsObject = catchCommon("JsObject"){
      json.as[JsObject]
    }

    def asString = catchCommon("String"){
      json.as[JsString].value
    }

    def asNumber = catchCommon("Number"){
      json.as[JsNumber].value
    }

    def asBoolean = catchCommon("Boolean"){
      json.as[JsBoolean].value
    }

    def isMap: Boolean = json.isInstanceOf[JsObject]
    def isNumber: Boolean = json.isInstanceOf[JsNumber]
    def isBoolean: Boolean = json.isInstanceOf[JsBoolean]
    def isString: Boolean = json.isInstanceOf[JsString]

    def isDefined: Boolean = true
    def hasKey(key: String): Boolean = catchCommon("hasKey"){
      json.as[JsObject].keys.contains(key)
    }
  }

  def string(s: String): Json = 
    JsonWithValue(JsString(s))

  def number(n: BigDecimal): Json = 
    JsonWithValue(JsNumber(n))

  def boolean(b: Boolean): Json = 
    JsonWithValue(JsBoolean(b))

  def jsValue(j: JsValue): Json =
    JsonWithValue(j)

  def parse(jsonString: String): Json =
    try{
      JsonWithValue(play.api.libs.json.Json.parse(jsonString))
    }catch{
      case e: Throwable => throw new JsonParseException(s"""Could not parse "$jsonString": ${e.getMessage()}""")
    }

  def parseAndValidate[R](jsonString: String, ignore: Set[String] = Set(), allowedKeys: Set[String] = Set())(command: Json => R): R = {
    val json = parse(jsonString)

    if (allowedKeys != Set() && json.isMap)
      json.asMap.keys.foreach(key =>
        if (!allowedKeys.contains(key))
          throw new Exception(s"""Key "$key" is not supported when parsing json string""")
      )

    val ret = command(json)
    json.validateRemaining(ignore)
    ret
  }

  // Same as parseAndValidate, except that it doesn't validate that all fields are read.
  def parseIt[R](jsonString: String)(command: Json => R): R =
    command(parse(jsonString))

  def flattenJsObjects(objs: Seq[JsObject]): JsObject =
    if (objs.isEmpty)
      Json.obj()
    else
      objs.head ++ flattenJsObjects(objs.tail)
/*
  def flattenJsObjects(objs: JsObject*): JsObject =
    flattenJsObjects(objs.toSeq)
 */
}

