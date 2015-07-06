package no.samordnaopptak.apidoc


class JsonUtil {

  import play.api.libs.json._


  class JsonParseException(val message: String) extends Exception(message)

  /**
    All json parsing failures will throw this exception. The method may be overridden.
    */
  def createParseException(message: String): Exception =
    new JsonParseException(message)


  trait Json {

    private var visitedKeys: Set[String] = Set()

    def pp() = Json.prettyPrint(this.asJsValue)

    override def toString =
      "JsonUtil.Json(\n"+pp()+"\n)"

    def asDouble = asNumber.toDouble
    def asInt = asNumber.toInt
    def asLong = asNumber.toLong

    protected def error =
      throw createParseException(s"""Trying to access something illegal in JsonUtil.Json. This: ${pp()}""")

    def asJsValue: JsValue
    def asJsObject: JsObject = error
    def asJsArray: JsArray = error
    def asMap: Map[String, Json] = error
    def asArray: List[Json] = error
    def asString: String = error
    def asNumber: BigDecimal = error
    def asBoolean: Boolean = error

    def isArray: Boolean = false
    def isObject: Boolean = false
    def isNumber: Boolean = false
    def isBoolean: Boolean = false
    def isString: Boolean = false
    def isNull: Boolean = false

    def isDefined: Boolean = true
    def hasKey(key: String): Boolean = error  // it might be undefined even if it has key. (i.e. when the value is null)
    def keys: Set[String] = error
    def size: Int = error

    def ++(other: Json): Json = error

    def asLongArray = asArray.map(_.asLong)
    def asIntArray = asArray.map(_.asInt)
    def asDoubleArray = asArray.map(_.asDouble)
    def asStringArray = asArray.map(_.asString)
    def asBooleanArray = asArray.map(_.asBoolean)

    def asOption[R](command: Json => R): Option[R] =
      Some(command(this))

    def getOrElse[R](command: Json => R, orElseValue: R): R =
      command(this)

    def apply(key: String) =
      asMap.get(key) match {
        case Some(json) => {
          visitedKeys = visitedKeys + key
          json
        }
        case None =>
          JUndefined(key, this)
      }

    def apply(index: Int) =
      try{
        asArray(index)
      }catch{
        case e: java.lang.IndexOutOfBoundsException => throw createParseException("index "+index+" not found in "+pp()+" ("+e.getMessage()+")")
      }

    def validateRemaining(ignoreKeys: Set[String]): Unit = {
      val visitedKeys = this.visitedKeys ++ ignoreKeys
      val diff = asMap.keys.toSet.diff(visitedKeys)

      if (!diff.isEmpty)
        throw createParseException(s"""Unknown field(s): ${diff.mkString("\"", "\", \"", "\"")}""")
    }

    def validateRemaining(ignoreKeys: String*): Unit =
      validateRemaining(ignoreKeys.toSet)
  }


  case class JNumber(value: BigDecimal) extends Json{
    override def asNumber = value
    override def isNumber = true
    override def asJsValue = JsNumber(value)
  }

  case class JString(value: String) extends Json{
    override def asString = value
    override def isString = true
    override def asJsValue = JsString(value)
  }

  case class JBoolean(value: Boolean) extends Json{
    override def asBoolean = value
    override def isBoolean = true
    override def asJsValue = JsBoolean(value)
  }

  case class JObject(value: Map[String, Json]) extends Json{
    override def asMap: Map[String, Json] = value
    override def isObject = true
    override def asJsValue = asJsObject
    override def asJsObject = JsObject(
      value.map{
        case (k: String, v: Json) => k -> v.asJsValue
      }
    )
    override def size = value.size
    override def keys = value.keys.toSet
    override def hasKey(key: String) = keys.contains(key)

    override def ++(other: Json): Json =
      JObject(value ++ other.asMap)
  }

  case class JArray(value: List[Json]) extends Json{
    override def asArray = value
    override def isArray = true
    override def asJsValue = asJsArray
    override def asJsArray = JsArray(value.map(_.asJsValue))
    override def size = value.size

    override def ++(other: Json): Json =
      JArray(value ++ other.asArray)
  }

  case class JUndefined(key: String, parent: Json) extends Json{
    override def pp() = "<undefined>"
    override def error =
      throw createParseException("""Trying to access key """"+key+"""", which is not found in """+parent)
    override def asOption[R](command: Json => R): Option[R] = None
    override def getOrElse[R](command: Json => R, orElseValue: R): R = orElseValue
    override val isDefined = false
    override def asJsValue = JsNull
  }

  object JNull extends Json {
    override def asJsValue = JsNull
    override def isNull = true
    override def asOption[R](command: Json => R): Option[R] = None
    override def getOrElse[R](command: Json => R, orElseValue: R): R = orElseValue
    override val isDefined = false
  }

  private def jsValueToJson(value: JsValue): Json =
    value match {
      case j: JsNumber => JNumber(j.value)
      case j: JsString => JString(j.value)
      case j: JsBoolean => JBoolean(j.value)
      case j: JsObject => JObject(
        j.value.map{
          case (k: String, v: JsValue) => k -> jsValueToJson(v)
        }.toMap
      )
      case j: JsArray => JArray(j.value.map(jsValueToJson(_)).toList)
      case `JsNull` => JNull
    }

  def apply(a: Any): Json =
    a match {
      case value: Json =>  value
      case value: JsValue =>  jsValueToJson(value)
      case value: BigDecimal =>  JNumber(value)
      case value: Int =>  JNumber(value)
      case value: Long =>  JNumber(value)
      case value: Float =>  JNumber(value)
      case value: Double =>  JNumber(value)
      case value: String => JString(value)
      case value: Boolean => JBoolean(value)
      case value: Map[_,_] => JObject(
        value.asInstanceOf[Map[String,Any]].map{
          case (k: String, v: Any) => k -> apply(v)
        }
      )
      case value: Seq[_] => JArray(value.map(apply(_)).toList)
    }

  def obj(vals: (String, Any)*): Json = {
    apply(vals.toMap)
  }

  def arr(vals: Any*): Json =
    apply(vals.map(apply))

  def parse(jsonString: String): Json =
    try{
      jsValueToJson(play.api.libs.json.Json.parse(jsonString))
    }catch{
      case e: Throwable => throw createParseException(s"""Could not parse "$jsonString": ${e.getMessage()}""")
    }

  def parseAndValidate[R](jsonString: String, ignore: Set[String] = Set(), allowedKeys: Set[String] = Set())(command: Json => R): R = {
    val json = parse(jsonString)

    if (allowedKeys != Set() && json.isObject)
      json.asMap.keys.foreach(key =>
        if (!allowedKeys.contains(key))
          throw createParseException(s"""Key "$key" is not supported when parsing json string""")
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

