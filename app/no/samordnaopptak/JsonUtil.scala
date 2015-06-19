package no.samordnaopptak.apidoc


class JsonUtil {

  import play.api.libs.json._


  private def wrapperToJsValue(wrapper: Json.JsValueWrapper) =
    Json.arr(wrapper).value(0)

  class JsonParseException(val message: String) extends Exception(message)

  trait Json{
    private var visitedKeys: Set[String] = Set()

    def pp(): String

    override def toString =
      "JsonUtil.Json(\n"+pp()+"\n)"

    def asJsValue: JsValue
    def asJsObject: JsObject
    def asMap: Map[String, Json]
    def asList: List[Json]
    def asString: String
    def asBoolean: Boolean
    def asNumber: BigDecimal

    def asDouble = asNumber.toDouble
    def asInt = asNumber.toInt
    def asLong = asNumber.toLong

    def isList: Boolean
    def isMap: Boolean
    def isNumber: Boolean
    def isBoolean: Boolean
    def isString: Boolean

    def isDefined: Boolean
    def hasKey(key: String): Boolean // it might be undefined even if it has key. (i.e. when the value is null)

    def keys: Set[String]

    def size: Int

    def ++(other: Json): Json =
      map(asMap ++ other.asMap)

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

    def asJsValue = JsNull
    def asJsObject = error
    def asMap = error
    def asList = error
    def asString = error
    def asNumber = error
    def asBoolean = error

    def isList = error    
    def isMap = error
    def isNumber: Boolean = error
    def isBoolean: Boolean = error
    def isString: Boolean = error

    def isDefined: Boolean = false
    def hasKey(key: String): Boolean = false

    def keys: Set[String] = error

    def size: Int = error

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

    lazy val asMap: Map[String, Json] = catchCommon("Map"){
      json.as[JsObject].value.map{
        case (key, `JsNull`) => key -> JsonWithoutValue(key, this)
        case (key, value) => key -> JsonWithValue(value)
      }.toMap
    }

    lazy val asList = catchCommon("List"){
      json.as[JsArray].value.toList.map(JsonWithValue(_))
    }

    def asJsValue = json

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

    def isList: Boolean = json.isInstanceOf[JsArray]
    def isMap: Boolean = json.isInstanceOf[JsObject]
    def isNumber: Boolean = json.isInstanceOf[JsNumber]
    def isBoolean: Boolean = json.isInstanceOf[JsBoolean]
    def isString: Boolean = json.isInstanceOf[JsString]

    def isDefined: Boolean = true
    def hasKey(key: String): Boolean = catchCommon("hasKey"){
      json.as[JsObject].keys.contains(key)
    }

    def keys: Set[String] = asMap.keys.toSet

    def size: Int =
      if (isList)
        json.as[JsArray].value.size
      else if (isMap)
        json.as[JsObject].value.size
      else
        throw new JsonParseException("'" + pp() + s"' is not an array or a map")
  }

  def string(s: String): Json =
    JsonWithValue(JsString(s))

  def number(n: BigDecimal): Json = 
    JsonWithValue(JsNumber(n))

  def boolean(b: Boolean): Json = 
    JsonWithValue(JsBoolean(b))

  def jsValue(j: JsValue): Json =
    JsonWithValue(j)

  def map(m: Map[String, Any]): Json =
    jsValue(
      Json.toJson(
        m.map {
          case (key, value: Json) => key -> value.asJsValue
          case (key, value: JsValue) => key -> value
          case (key, value: Int) => key -> JsNumber(value) // todo: Make a general any->json function, somehow.
          case (key, value) => key -> wrapperToJsValue(value.asInstanceOf[Json.JsValueWrapper])
        }
      )
    )

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


object JsonUtil extends JsonUtil

