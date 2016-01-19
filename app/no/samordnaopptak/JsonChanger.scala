package no.samordnaopptak.json

import play.api.libs.json.JsString


class JsonChangerException(message: String, val path: String) extends Exception(message)

/**
  * Change the content of a json value in a safe and descriptive manner.
  * 
  * Firstly, the name is misleading. JsonChanger is not ''changing'' (i.e mutating) the values. It returns a new value.
  * 
  * The main function, [[JsonChanger.apply]] takes two arguments: '''json_value''' and '''changer'''.
  * The '''changer''' variable has similarities to the '''matcher''' variable used in [[JsonMatcher]], but while
  * [[JsonMatcher]] only returns '''true''' or '''false''', JsonChanger returns a new Json value.
  * 
  * The most important similarity between JsonChanger and [[JsonMatcher]] is that they both do pattern matching.
  * When changing a json value with JsonChanger, it also pattern matches the '''json_value''' against the '''changer'''. This pattern matching
  * should make bugs appear earlier than they would have been othervice.
  * (You might argue that it would be better to first validate the json against a schema, but this way you get validation for free, plus that the validation schema maintains itself automatically.)
  * 
  * There are several custom changers such as [[___identity]], [[Replace]], [[Func]], [[Map]], [[MapChanger]], etc. See documentation below for examples.
  * Custom changers can be also be created from the outside of JsonChanger by implementing the [[Changer]] trait.
  * 
  * The pattern matcher in JsonChanger checks that: a) a json value doesn't change type (unless we tell it to),
  * b) we don't add or remove fields to objects (unless we tell it to), c) we don't add or remove values to arrays (unless we tell it to).
  * 
  * 
  * @example
  {{{
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
  }}}
  * 
  * @see [[https://github.com/sun-opsys/doppelauge/blob/master/test/JsonChangerSpec.scala]] for more examples
  */
object JsonChanger{

  private def throwChangeException(message: String, path: String) =
    throw new JsonChangerException(message+"\n\npath: "+path+"\n\n ", path)


  /**
    * Super class for changers. Custom Changers can be made and used from the outside of JsonChanger.
    * 
    * @example
    * This is the actual implementation of the [[JsonChanger.Func]] changer:
    {{{
  case class Func(func: JValue => Any) extends Changer {
    override def pp() = "Func"

    def transformer(json: JValue, path: String, allow_mismatched_types: Boolean) =
      JsonChanger.apply(json, func(json), path, allow_mismatched_types)
  }
    }}}
    */
  trait Changer extends JValue {
    override def asJsValue = JsString(pp())

    /**
      The transformer virtual method.
      @param allow_mismatched_types Must be forwarded if calling [[JsonChanger.apply]], unless you want to explicitly override type mismatching to fail or succeed (see [[AllowMismatchedTypes]]).
      */
    def transformer(json: JValue, path: String, allow_mismatched_types: Boolean): Any
  }

  /**
    * Apply a custom function on a json value
    * 
    * @example
    {{{
      JsonChanger(
        J.obj(
          "aaa" -> 30
        ),
        J.obj(
          "aaa" -> JsonChanger.Func(_ + 50)
        )
      ) ===
      J.obj("aaa" -> 80)
    }}}
    */
  case class Func(func: JValue => Any) extends Changer {
    override def pp() = "Func"

    def transformer(json: JValue, path: String, allow_mismatched_types: Boolean) =
      JsonChanger.apply(json, func(json), path, allow_mismatched_types)
  }

  /**
    * Change the name and content of a field
    * 
    * @example
    * {{{
      JsonChanger(
        J.obj(
          "aaa" -> 50
        ),
        J.obj(
          "aaa" -> JsonChanger.ChangeThisField(
                     "bbb" -> JsonChanger.___identity
                   )
        )
      ) ===
      J.obj("bbb" -> 50)
    * }}}
    * 
    * ChangeThisField can also be surrounded with [[Maybe]]:
    * {{{
      JsonMatcher.matchJson(
        JsonChanger(
          J.obj(),
          J.obj(
            "aaa" -> JsonChanger.Maybe(
                       JsonChanger.ChangeThisField(
                         "bbb" -> JsonChanger___identity
                       )
                    )
          )
        ),
        J.obj()
      )
    * }}}
    * Othervice, an exception is thrown if the field doesn't exist. If you want to create a new field no matter what, a combination of [[JsonChanger.Maybe]]([[___removeThisField]]) and [[NewField]] can be used instead.
    */ 
  case class ChangeThisField(newFieldNameAndChanger: (String, Any)) extends JValue {
    val newFieldName = newFieldNameAndChanger._1
    val j_changer = J(newFieldNameAndChanger._2)
    override def pp() = "ChangeThisField: "+newFieldName+", "+j_changer.pp()
    override def asJsValue = JsString(pp())
  }


  /**
    * Add new field to object.
    * @note If the field may already exist in the object, use [[ForceNewField]] instead.
    * 
    * @example
    {{{
        JsonChanger(
          J.obj(
            "aaa" -> 50
          ),
          J.obj(
            "aaa" -> 90,
            "bbb" -> JsonChanger.NewField(100)
          )
        ) ===
        J.obj(
          "aaa" -> 90,
          "bbb" -> 100
        )
    }}}
    * 
    */
  case class NewField(value: Any) extends JValue {
    val j_value = J(value)
    override def pp() = "NewField: "+j_value.pp()
    override def asJsValue = JsString(pp())
  }


  /**
    * Same as [[NewField]], but does not throw exception if the field already exists. The argument is also a changer and not a value.
    * If the key doesn't exist, the input value to the changer will be an instance of [[JUndefined]].
    * 
    *  @example
    {{{
      JsonMatcher.matchJson(
        JsonChanger(
          J.obj(
            "aaa" -> "hello"
          ),
          J.obj(
            "aaa" -> JsonChanger.ForceNewField(JsonChanger.Func(_.getOrElse("[undefined]"))),
            "bbb" -> JsonChanger.ForceNewField(JsonChanger.Func(_.getOrElse("[undefined]")))
          )
        ),
        J.obj(
          "aaa" -> "hello",
          "bbb" -> "[undefined]"
        )
      )
    }}}
    * 
    */
  case class ForceNewField(changer: Any) extends JValue {
    override def pp() = "ForceNewField("+J(changer).pp()+")"
    override def asJsValue = JsString(pp())
  }


  /**
    * Apply a change to a json value if the json value is defined.
    * 
    * @note Must be used if it is uncertain whether a field is present in an object.
    * 
    * @example
    {{{
        JsonChanger(
          J.obj(
            "aaa" -> 30
          ),
          J.obj(
            "aaa" -> JsonChanger.Maybe(50)
          )
        ) ===
        J.obj(
          "aaa" -> 50
        )

        JsonChanger(
          J.obj(),
          J.obj(
            "aaa" -> JsonChanger.Maybe(50)
          )
        ) ===
        J.obj()

        JsonChanger(
          null,
          JsonChanger.Maybe(JsonChanger.Func(_ + 1))
        ) ===
        JNull

        JsonChanger(
          90,
          JsonChanger.Maybe(JsonChanger.Func(_ + 1))
        ) ===
        91
      )

    }}}
    * 
    */
  case class Maybe(changer: Any) extends Changer {
    val j_changer = J(changer)
    override def pp() = "Maybe("+j_changer.pp()+")"

    def transformer(json: JValue, path: String, allow_mismatched_types: Boolean) =
      if (json.isDefined)
        JsonChanger.apply(json, changer, path, allow_mismatched_types)
      else
        json
  }


  /**
    * Replaces the input with a specified output if the input has a specified value.
    * 
    * @example
    {{{
        JsonChanger(
          J.arr(2,3),
          J.arr(JsonChanger.Replace(3, 5), JsonChanger.Replace(3, 5))
        ) ===
        J.arr(2, 5)
    }}}
    * 
    */
  case class Replace(comparison_value: Any, to_changer: Any) extends Changer {
    val j_comparison_value = J(comparison_value)
    val j_to_changer = J(to_changer)

    override def pp() = "JsonChanger.Replace(comparison: "+j_comparison_value.pp()+", to: "+j_to_changer.pp()+")"

    def transformer(json: JValue, path: String, allow_mismatched_types: Boolean) =
      if (json==j_comparison_value)
        JsonChanger.apply(json, j_to_changer, path, allow_mismatched_types)
      else
        json
  }

  /**
    * Used in conjunction with [[AllowMismatchedTypes]]
    */
  object Expects extends Enumeration {
    type Type = Value

    /**
      * Expects object
      */
    val Object = Value

    /**
      * Expects array
      */
    val Array = Value

    /**
      * Expects number
      */
    val Number = Value

    /**
      * Expects null
      */
    val Null = Value

    /**
      * Expects string
      */
    val String = Value

    /**
      * Expects boolean
      */
    val Boolean = Value

    /**
      * Expects undefined value. An undefined value is either null, or the result of trying to do e.g.
      * {{{
      J.obj()("hello")
      * }}}
      */
    val Undefined = Value

    /**
      * Expects a defined value. Defined values are all values that are not undefined
      * @see [[Undefined]]
      */
    val Defined = Value

    /**
      * Expects anything, both defined and undefined values
      * @see [[Defined]] and [[Undefined]]
      */
    val Anything = Value
  }


  /**
    *  Bypasses pattern matching
    * 
    * @example
    {{{
      JsonChanger(
        50,
        JsonChanger.AllowMismatchedTypes(JsonChanger.Expects.Number, "aiai")
      ) === JString("aiai")
    }}}
    * 
    * @param expectedType the input value must match expectedType. If the value type is unknown, [[Expects.Defined]] or [[Expects.Anything]] can be used.
    * @see [[Expects]]
    */
  case class AllowMismatchedTypes(expectedType: Expects.Type, changer: Any) extends Changer {
    override def pp() = "AllowMismatchedTypes. changer: "+J(changer).pp()

    import Expects._

    private def validateValue(json: JValue, path: String): Unit = {

      def maybeThrow(maybe: Boolean) =
        if (!maybe)
          throwChangeException("JsonChanger.AllowMismatchedTypes expected "+expectedType+", but found "+json.pp(), path)

      expectedType match {
        case Object    => maybeThrow(json.isObject)
        case Array     => maybeThrow(json.isArray)
        case Number    => maybeThrow(json.isNumber)
        case Null      => maybeThrow(json.isNull)
        case String    => maybeThrow(json.isString)
        case Boolean   => maybeThrow(json.isBoolean)
        case Undefined => maybeThrow(!json.isDefined)
        case Defined   => maybeThrow(json.isDefined)
        case Anything  => ()
      }
    }

    def transformer(json: JValue, path: String, allow_mismatched_types: Boolean) = {
      validateValue(json, path)
      JsonChanger.apply(json, changer, path, true)
    }
  }


  /**
    * Map a function on array
    * 
    * @example
    {{{
        JsonChanger(
          J.arr(2,3),
          JsonChanger.Map(_ + 2)
        ) ===
        J.arr(4,5)
    }}}
    * 
    */
  case class Map(transformer_func: JValue => Any) extends Changer {
    override def pp() = "Map"

    // Perhaps Map should do type checking on the individual fields?

    def transformer(json: JValue, path: String, allow_mismatched_types: Boolean) =
      if (json.isArray)
        json.asArray.map(transformer_func)
      else
        throwChangeException("JsonChanger.Map expects array. Found "+json.pp(), path)
  }


  /**
    * Map [[JsonChanger.apply]] on array
    * 
    * @example
    {{{
        JsonChanger(
          J.arr(2,3),
          JsonChanger.MapChanger(JsonChanger.Replace(3,9))
        ) ===
        J.arr(2,9)
    }}}
    * 
    */
  case class MapChanger(changer: Any) extends Changer {
    val j_changer = J(changer)
    override def pp() = "MapChanger "+j_changer.pp()

    private def change(n: Int, jsons: List[JValue], path: String): List[JValue] =
      if (jsons.isEmpty)
        List()
      else
        JsonChanger.apply(jsons.head, j_changer, path + "[" + n + "]", false) :: change(1+n, jsons.tail, path)

    def transformer(json: JValue, path: String, allow_mismatched_types: Boolean) =
      if (json.isArray)
        J(change(0, json.asArray.toList, path))
      else
        throwChangeException("JsonChanger.MapChanger expects array. Found "+json.pp(), path)

  }


  /**
    * Allow other fields in an object than the ones specified in the changer.
    * 
    * @example
    {{{
        JsonChanger(
          J.obj(
            "aaa" -> 50
          ),
          J.obj(
            JsonChanger.___allowOtherFields
          )
        ) ===
        J.obj("aaa" -> 50)
    }}}
    * 
    */
  val ___allowOtherFields  = JsonMatcher.___allowOtherFields

  /**
    * Allow other values in an array than the ones specified in the changer.
    * Must be specified at the last position of a changer array.
    * 
    * @example
    {{{
        JsonChanger(
          J.arr(5),
          J.arr(
            JsonChanger.___allowOtherValues
          )
        ) ===
        J.arr(5)
    }}}
    * 
    */
  val ___allowOtherValues  = JsonMatcher.___allowOtherValues

  /**
    * Removes a field from an object
    * 
    * @example
    * {{{
        JsonChanger(
          J.obj(
            "aaa" -> 50
          ),
          J.obj(
            "aaa" -> JsonChanger.___removeThisField
          )
        ) ===
        J.obj()
    * }}}
    * 
    * [[___removeThisField]] must be surrounded with [[Maybe]] if it's uncertain whether the field is present, in order to avoid [[JsonChangerException]] to be thrown:
    * {{{
      JsonMatcher.matchJson(
        JsonChanger(
          J.obj(),
          J.obj(
            "aaa" -> JsonChanger.Maybe(JsonChanger.___removeThisField)
          )
        ),
        J.obj()
      )
    * }}}
    */
  val ___removeThisField = JString("_____________removeThisField___________")


  /**
    * A [[Func]] instance to return the input
    * 
    * Implementation:
    {{{val ___identity = Func(a => a)}}}
    * 
    * @example
    {{{
        JsonChanger(
          J.obj(
            "aaa" -> 10
          ),
          J.obj(
            "aaa" -> JsonChanger.___identity
          )
        ) === J.obj("aaa" -> 10)
    }}}
    * 
    */
  val ___identity = Func(a => a)


  /**
    * Insert new value into array
    * @example
    {{{
        JsonChanger(
          J.arr(
            2,
            3
          ),
          J.arr(
            JsonChanger.___identity,
            JsonChanger.InsertValue(90),
            JsonChanger.___identity
          )
        ) ===
        J.arr(
          2,
          90,
          3
        )
    }}}
    * @note see also [[___removeValue]]
    */
  case class InsertValue(value: Any) extends JValue {
    val j_value = J(value)
    override def pp() = "InsertValue "+j_value.pp()
    override def asJsValue = JsString(pp())
  }


  /**
    * Remove value from array
    * @example
    {{{
        JsonChanger(
          J.arr(
            2,
            3
          ),
          J.arr(
            JsonChanger.___removeValue,
            JsonChanger.___identity
          )
        ) ===
        J.arr(
          3
        )
    }}}
    * @note see also [[InsertValue]]
    */
  val ___removeValue = JString("___________________removeValue________________")


  private def changeArray(json: JArray, changer: JArray, path: String): JValue = {

    val allowOthers = J(___allowOtherValues)

    def change(n: Int, jsons: List[JValue], changers: List[JValue]): List[JValue] = (jsons, changers) match {
      case (Nil, Nil) => List()

      case (_, (insert: InsertValue) :: rest) =>
        insert.j_value :: change(n+1, jsons, rest)

      case (Nil, _) =>
        throwChangeException("Too many changers in array: "+J(changers).pp(), path)

      case (_, Nil) =>
        throwChangeException("Missing changer for "+jsons.head.pp(), path + "[" + n + "]")

      case (_, `allowOthers` :: Nil) =>
        jsons

      case (_, `allowOthers` :: rest) =>
        throwChangeException("Can not have values after ___allowOtherValues in an array: "+json.pp(), path)

      case (j::js, `___removeValue` :: rest) =>
        change(n, js, rest)

      case (j::js, c::cs) =>
        apply(j, c, path + "[" + n + "]") :: change(1+n, js, cs)
    }

    J(
      change(0, json.value.toList, changer.value.toList)
    )
  }

  // Needs cleanup
  private def changeObject(json: JObject, changer: JObject, path: String): JValue = {

    val allowOtherFields = changer.asMap.get(___allowOtherFields._1) != None

    def applyChanger(key: String, value: JValue, changer: JValue): (String, JValue) = changer match {
      case changeThisField: ChangeThisField =>
        applyChanger(changeThisField.newFieldName, value, changeThisField.j_changer)

      case newFieldChanger: NewField => throwChangeException("""NewField can not be used on existing field. Must use "ForceNewField" instead.""", path+"."+key)

      case forceNewField: ForceNewField =>
        key -> apply(value, forceNewField.changer, path+"."+key)

      case maybeChanger: Maybe =>
        if (value.isDefined)
          applyChanger(key, value, maybeChanger.j_changer)
        else
          key -> value

      case `___removeThisField` =>
        key -> changer

      case changer: JValue =>
        key -> apply(value, changer, path+"."+key)
    }

    var usedChangerKeys = Set[String]()

    // Create return value
    //
    val ret = json.asMap.map{
      case (key: String, value: JValue) => changer.asMap.get(key) match {

        case None =>
          if (allowOtherFields)
            key -> value
          else
            throwChangeException("No changer for key \""+key+"\" in "+changer.pp(), path)

        case Some(changer) => {
          val res = applyChanger(key, value, changer)
          usedChangerKeys += res._1
          res
        }

      }
    }.filter(field =>
      field._2 != ___removeThisField
    )

    val addedFieldsInChanger = changer.asMap.filter {
      case (key: String, _: NewField)      => true
      case (key: String, _: ForceNewField) => !ret.keys.toSet.contains(key)
      case _ => false
    }.map{
      case (key: String, newField: NewField)           => key -> newField.value
      case (key: String, forceNewField: ForceNewField) => key -> apply(JUndefined(key, json), forceNewField.changer, path+"."+key, true)
    }

    // Check that all changer keys are used (if there are bugs, it's probably better to rewrite than try to fix it)
    if(true){
      val changerKeys = changer.asMap.map{
        case (key: String, changeThisField: ChangeThisField) => changeThisField.newFieldName
        case (key: String, _) => key
      }.toSet

      val maybeChangerKeys = changer.asMap.filter{
        case (key: String, value: Maybe) => true
        case (key: String, changeThisField: ChangeThisField) if changeThisField.j_changer.isInstanceOf[Maybe] => true
        case _ => false
      }.map{
        case (key: String, changeThisField: ChangeThisField) => changeThisField.newFieldName
        case (key: String, _) => key
      }.toSet

      //println("changerKeys: "+changerKeys)
      //println("usedChangerKeys: "+usedChangerKeys)
      //println("maybeChangerKeys: "+maybeChangerKeys)

      val unusedChangerKeys = changerKeys -- usedChangerKeys -- maybeChangerKeys -- Set(___allowOtherFields._1) -- addedFieldsInChanger.map(_._1)

      if ( unusedChangerKeys.size > 0)
        throwChangeException("Unknown keys in changer: "+unusedChangerKeys.mkString, path)
    }

    J(ret) ++ J(addedFieldsInChanger)
  }


  private def apply(json: JValue, changer: JValue, path: String, allow_mismatched_types: Boolean): JValue = {

    //println("JsonChanger.apply. path: "+path)

    def throwMatchException(match_type: String) =
      throwChangeException(s"$json is not a $match_type. You can use AllowMismatchedTypes on the changer to avoid this. value: "+json.pp()+", changer: "+changer.pp(), path)

    (json, changer, allow_mismatched_types) match {

      case (_,           c: Changer,   _)    => J(c.transformer(json, path, allow_mismatched_types))

      case (j: JObject,  c: JObject,   _)    => changeObject(j, c, path)
      case (j: JArray,   c: JArray,    _)    => changeArray(j, c, path)

      case (_,           _,            true) => changer

      case (_: JNumber,    c: JNumber,    _) => c
      case (_: JString,    c: JString,    _) => c
      case (_: JBoolean,   c: JBoolean,   _) => c
      case (_: JUndefined, c: JUndefined, _) => c

      case (`JNull`,     `JNull`,        _)  => JNull
      case (_,           `JNull`,        _)  => throwMatchException("null")

      case (_,           _: JObject,     _)  => throwMatchException("object")
      case (_,           _: JArray,      _)  => throwMatchException("array")
      case (_,           _: JNumber,     _)  => throwMatchException("number")
      case (_,           _: JString,     _)  => throwMatchException("string")
      case (_,           _: JBoolean,    _)  => throwMatchException("boolean")
      case (_,           _: JUndefined,  _)  => throwMatchException("undefined")

      case (_,           _,              _)  => changer
    }

  }

  /**
    * Main function.
    * @param json_value the value to change
    * @param changer the pattern to change against
    * @param path Included at the start of the '''path''' value in [[JsonChangerException]]
    * @param allow_mismatched_types Allow mismatched types. Example:
    {{{
     
      // This example:

      apply(50, "hello", allow_mismatched_types = true)

      // ...works. However, this example:

      apply(J.arr(50), J.arr("hello"), allow_mismatched_types = true)

      // ...will fail. To allow mismatched types for array or object values, we must do this instead:

      apply(J.arr(50), J.arr(AllowMismatchedTypes("hello"))
    }}}
    * 
    The '''allow_mismatched_types''' parameter is exposed here since it must be handled manually in [[Changer.transformer]]. In [[Changer.transformer]], you sometimes want to call [[JsonChanger.apply]] and then '''allow_mismatched_types''' must be forwarded to avoid the changer to fail if the current [[Changer]] instance (i.e. '''this''') was surrounded with an [[AllowMismatchedTypes]] changer.
    */
  def apply(json_value: Any, changer: Any, path: String = "", allow_mismatched_types: Boolean = false): JValue =
    apply(J(json_value), J(changer), path, allow_mismatched_types)

}
