package no.samordnaopptak.apidoc

import scala.reflect.ClassTag

import com.google.inject.Inject

import no.samordnaopptak.json._

import no.samordnaopptak.test.TestByAnnotation.Test


object ApiDocParser {

  trait ApiDocElement {
    def toJson: JValue
  }

  case class MethodAndUri(method: String, uri: String, uriParms: List[String]) extends ApiDocElement {
    def toJson = J.obj(
      "method" -> method,
      "uri"    -> uri,
      "uriParms" -> uriParms
    )
  }

  case class Description(shortDescription: String, longDescription: Option[String]) extends ApiDocElement {
    def toJson = J.obj(
      "shortDescription" -> shortDescription,
      "longDescription"  -> longDescription.getOrElse("")
    )
  }

  /**
    * A ParamType can either be body, path, query, header, formData, or undefined.
    */
  object ParamType extends Enumeration{
    type Type = Value
    val body, path, query, header, formData, undefined = Value

    def fromString(string: String): Type =
      string match {
        case "body" => body
        case "path" => path
        case "query" => query
        case "header" => header
        case "formData" => formData
        case _ => throw new Exception(s""""$string" is not a valid parameter type. It must be either "body", "path", "query", "header", or "formData". See https://github.com/wordnik/swagger-core/wiki/Parameters""")
      }

    def toJson(paramType: Type) =
      if (paramType==undefined)
        JNull
      else
        JString(paramType.toString)
  }

  case class Field(name: String, type_ : String, paramType: ParamType.Type, isArray: Boolean, enumArgs: List[String], required: Boolean, comment: Option[String]) extends ApiDocElement {
    val isEnum = enumArgs.nonEmpty

    if(type_.count(_.isWhitespace) > 0)
      throw new Exception("A type name can not contain white space: '"+ type_ +"'")

    def usedDataTypes: Set[String] = Set(type_)

    def toJson = J.obj(
      name -> J.obj(
        "type" -> type_ ,
        comment match {
          case None          => "noComment" -> true
          case Some(commented) => "comment" -> commented
        },
        "isArray" -> isArray,
        "isEnum" -> isEnum,
        "enumArgs" -> enumArgs,
        "paramType" -> ParamType.toJson(paramType),
        "required"  -> required
      )
    )
  }

  case class Parameters(fields: List[Field]) extends ApiDocElement{
    def fieldNames = fields.map(_.name).toSet

    def usedDataTypes: Set[String] =
      fields.flatMap(_.usedDataTypes).toSet

    def toJson = J.flattenJObjects(fields.map(_.toJson))
  }

  case class Error(code: Int, message: String) extends ApiDocElement{
    def toJson = J.obj(
      "code" -> code,
      "message" -> message
    )
  }

  case class Errors(errors: List[Error]) extends ApiDocElement{
    def toJson = JArray(errors.map(_.toJson))

    println("Warning, \"ERRORS\" is deprecated. Put error results into \"RESULT\" instead")
  }

  case class Result(code: Int, field: Field) extends ApiDocElement{
    assert(field.name == "result")

    def usedDataTypes: Set[String] = field.usedDataTypes

    def toJson = {
      val fieldJson = field.toJson
      J.obj(
        code.toString -> fieldJson("result")
      )
    }
  }

  case class Results(results: List[Result]) extends ApiDocElement{
    val codes = results.map(_.code)

    if (codes.distinct.size != codes.size)
      throw new Exception("One or more ApiDoc RESULT codes defined more than once: "+codes.diff(codes.distinct).take(4) + ", Result data: "+results.map(_.toJson))

    def usedDataTypes: Set[String] = results.flatMap(_.usedDataTypes).toSet

    def toJson = J.obj(
      "results" -> J.flattenJObjects(results.map(_.toJson))
    )
  }

  case class DataType(name: String, parameters: Parameters) extends ApiDocElement{
    def usedDataTypes: Set[String] = parameters.usedDataTypes

    def toJson = J.obj(
      name -> parameters.toJson
    )
  }

  case class DataTypes(dataTypes: List[DataType]) extends ApiDocElement{
    val names = dataTypes.map(_.name)
    if (names.size != names.distinct.size)
      throw new Exception("One or more ApiDoc datatypes defined more than once: "+names.diff(names.distinct).take(4))

    def usedDataTypes: Set[String] =
      dataTypes.flatMap(_.usedDataTypes).toSet

    def toJson = J.flattenJObjects(dataTypes.map(_.toJson))
  }

  case class ApiDoc(methodAndUri: MethodAndUri, description: Description, parameters: Option[Parameters], errors: Option[Errors], results: Option[Results]) extends ApiDocElement{

    def usedDataTypes: Set[String] =
      Set() ++
    (parameters match {
      case None => Set()
      case Some(parameter) => parameter.usedDataTypes
    }) ++
    (results match {
      case None => Set()
      case Some(result) => result.usedDataTypes
    })

    private def addMaybe(apiDoc: Option[ApiDocElement], key: String = "") =
      (apiDoc, key) match {
        case (None,    _)   => J.obj()
        case (Some(a), "")  => a.toJson
        case (Some(a), jsonKey) => J.obj(jsonKey -> a.toJson)
      }

    /**
      * Implementation:
      * {{{
      def validate() =
      ApiDocValidation.validate(this)
      * }}}
      */
    def validate(apiDocValidation: ApiDocValidation) =
      apiDocValidation.validate(this)

    def toJson =
      methodAndUri.toJson ++
      description.toJson ++
      addMaybe(parameters, "parameters") ++
      addMaybe(errors, "errors") ++
      addMaybe(results)
  }

  case class ApiDocs(apiDocs: List[ApiDoc]) extends ApiDocElement{
    def usedDataTypes = apiDocs.flatMap(_.usedDataTypes)

    /**
      * Implementation:
      * {{{
    def validate = apiDocs.foreach(_.validate())
      * }}}
      */
    def validate(apiDocValidation: ApiDocValidation) =
      apiDocs.foreach(_.validate(apiDocValidation))

    def toJson = JArray(apiDocs.map(_.toJson))
  }

  private def getIndentLength(line: String) =
    line.prefixLength(_==' ')

  @Test(code="""
    self.getEnumArgs("String") === (List(),0)
    self.getEnumArgs("String(query)") === (List(),0)
    self.getEnumArgs("Array String(query)") === (List(),0)
    self.getEnumArgs("Enum() String(query)") === (List(),6)
    self.getEnumArgs("Enum(1,2,3) String") === (List("1","2","3"),11)
    self.getEnumArgs("Enum(2,65,9) Int(query)") === (List("2","65","9"), 12)
  """)
  private def getEnumArgs(typetypetype: String): (List[String],Int) = {
    if (!typetypetype.startsWith("Enum ") && !typetypetype.startsWith("Enum("))
      return (List(),0)

    val startpos = typetypetype.indexOf('(')
    val endpos = typetypetype.indexOf(')')

    /*
    println("typetypetype: "+typetypetype)
    println("startpos: "+startpos)
    println("endpos: "+endpos)
     */

    val argsstring = typetypetype.substring(startpos+1,endpos)
    val args = argsstring.split(",").map(_.trim).filter(_ != "").toList
    //println("args: "+args)

    (args,endpos+1)
  }


  @Test(code="""
    self.findUriParms("/api/v1/acl/{service}")        === List("service")
    self.findUriParms("/api/v1/acl/{service}/{hest}") === List("service", "hest")
    self.findUriParms("/api/v1/acl/")                 === List()
    self.findUriParms("/api/v1/acl")                  === List()
  """)
  private def findUriParms(autoUri: String): List[String] =
    if (autoUri=="")
      List()
    else if (autoUri.startsWith("{")) {
      val next = autoUri.indexOf('}')
      autoUri.substring(1, next) :: findUriParms(autoUri.drop(next+1))
    } else
      findUriParms(autoUri.drop(1))



  @Test(code="""
      self.parseScalaTypeSignature("test.User(+a,-b)") === ("test.User", Set("a"), Set("b"))
    """)
  private def parseScalaTypeSignature(signature: String): (String, Set[String], Set[String]) = {

    val leftParPos = signature.indexOf('(')
    val rightParPos = signature.indexOf(')')

    if(leftParPos== -1 && rightParPos!= -1)
      throw new Exception("Malformed line: "+signature)
    if(leftParPos!= -1 && rightParPos== -1)
      throw new Exception("Malformed line: "+signature)
    if(leftParPos > rightParPos)
      throw new Exception("Malformed line: "+signature)

    if(leftParPos == -1) {

      (signature, Set(), Set())

    } else {

      val className = signature.take(leftParPos).trim
      val argsString = signature.substring(leftParPos+1, rightParPos).trim

      if (argsString=="") {

        (className, Set(), Set())

      } else {
        val modifiedFields = argsString.split(",").toList.map(_.trim)

        val addedFields = modifiedFields.filter(_.startsWith("+")).map(_.drop(1)).toSet
        val removedFields = modifiedFields.filter(_.startsWith("-")).map(_.drop(1)).toSet

        if (addedFields.size+removedFields.size != modifiedFields.size)
          throw new Exception("Malformed line: "+signature+". One or more modifier fields does not start with '-' or '+'")

        (className, addedFields, removedFields)
      }
    }
  }

  @Test(code="""
    self.testTypeInfo() === true
  """)
  private def testTypeInfo(): Boolean = {
    implicit class Hepp[T](val a: T) {
      def ===(b: T) = if (a!=b) throw new Exception(s"$a != $b") else true
    }

    TypeInfo("", "Array String (header)").type_  === "String"
    TypeInfo("", "Array String (header)").isArray === true
    TypeInfo("", "Array String (header)").paramType.toString === "header"

    TypeInfo("", "Enum(a,b) String (header)").type_  === "String"
    TypeInfo("", "Enum(a,b) String (header)").isEnum === true
    TypeInfo("", "Enum(a,b) String (header)").paramType.toString === "header"

    TypeInfo("", "Enum(2,65,9) Int(query)").type_  === "Int"
    TypeInfo("", "Enum(2,65,9) Int(query)").optional === false
    TypeInfo("", "Enum(2,65,9) Int(query,optional)").optional === true

    TypeInfo("", "String(header)").optional === false
    TypeInfo("", "String (header, optional)").optional === true
    TypeInfo("", "String(optional)").optional === true
    TypeInfo("", "String( optional)").optional === true
    TypeInfo("", "String").optional === false
  }

  private case class TypeInfo(val parmName: String, val typetypetype: String){                             // typetypetype = "Array String (header)"
    val (enumArgs,enumSize)  = getEnumArgs(typetypetype)
    val isArray      = typetypetype.startsWith("Array")
    val isEnum       = enumArgs.nonEmpty
    val typetype     = if (isArray) typetypetype.drop(6).trim else if (isEnum) typetypetype.drop(enumSize).trim else typetypetype                     // typetype = "String (header)"

    val leftParPos   = typetype.indexOf('(')
    val rightParPos  = typetype.indexOf(')')

    if (leftParPos>=0 && rightParPos == -1)
      throw new Exception(s"""Syntax error: Missing right paranthesis in "$parmName $typetypetype"""")

    if (leftParPos == -1 && rightParPos>=0)
      throw new Exception(s"""Syntax error: Missing left paranthesis in "$parmName $typetypetype"""")

    val hasTypeOptions = leftParPos != -1

    val type_        = if (hasTypeOptions) typetype.take(leftParPos).trim else typetype.trim           // type_ = "String"
    val typeOptions  = if (hasTypeOptions)
                          typetype.substring(leftParPos+1, rightParPos).split(',').map(_.trim).toSet
                       else
                         Set[String]()

    val optional     = typeOptions.contains("optional")

    val paramTypes   = typeOptions - "optional"

    if (paramTypes.size >= 2)
      throw new Exception(s"""Syntax error: Too many parameter options in "$parmName $typetypetype" ($paramTypes)""")

    val paramType    = if (paramTypes.size == 1) {                                                           // paramType = "header"
                          ParamType.fromString(paramTypes.head)
                       } else if (parmName=="body")
                         ParamType.body
                       else
                         ParamType.path
    /*
    println("parmName: "+parmName)
    println("typtyptyp: "+typetypetype)
     */

  }

  private case class Raw(key: String, elements: List[String]) {
    def plus(element: String) =
      Raw(key, elements ++ List(element))


    private def getParameters: Parameters =
      Parameters(
        elements.map(element => {
          if (element=="...")
            Field(
              name = "...",
              type_ = "etc.",
              isArray = false,
              enumArgs = List(),
              paramType = ParamType.undefined,
              required = false,
              comment = None
            )
          else {
            val nameLength = element.indexOf(':', 0)
            if(nameLength == -1)
              throw new Exception(s"Syntax error for element '$element'. (Missing ':')")
            val name = element.substring(0,nameLength)
            val rest = element.drop(nameLength+1).trim.split("<-")
            val typeInfo = TypeInfo(name, rest(0).trim)
            val comment = if (rest.length==1) "" else rest(1).trim

            Field(
              name = name,
              type_ = typeInfo.type_,
              comment = if (comment=="") None else Some(comment),
              isArray = typeInfo.isArray,
              enumArgs = typeInfo.enumArgs,
              paramType = typeInfo.paramType,
              required = !typeInfo.optional
            )
          }
        })
      )

    private def getResults(): Results =
      Results(
        elements.map(element => {

          val splitted1 = element.trim.split(":")

          val (code, varnameAndComment) =
            if (splitted1.size == 1)
              (200, element)
            else
              (splitted1(0).trim.toInt, splitted1.tail.mkString.trim)

          val splitted2 = varnameAndComment.split("<-").map(_.trim)

          val typeInfo = TypeInfo("", splitted2(0))
          val comment = if (splitted2.length==1) "" else splitted2(1)

          Result(
            code,
            Field(
              name = "result",
              type_ = typeInfo.type_,
              paramType = ParamType.undefined,
              isArray = typeInfo.isArray,
              enumArgs = typeInfo.enumArgs,
              required = true,
            comment = Some(comment)
            )
          )
        })
      )

    /*
     User: test.User(+a,-b)
     ...
     
     ->

     User -> {...}
     */
    private def parseDataType(apiDocValidation: ApiDocValidation, line: String): DataType = {
      val parameters = getParameters
      val fieldNames = parameters.fieldNames

      val (dataTypeName, signature) = if (line.endsWith(":")) {

        val signature = key.dropRight(1).trim
        val splitPos = line.indexOf('(')

        if (splitPos== -1)
          (signature, signature+"()")
        else
          (line.take(splitPos).trim, signature)

      } else {

        val splitPos = line.indexOf(':')
        val dataTypeName = line.take(splitPos).trim
        val signature = line.drop(splitPos+1).trim

        (dataTypeName, signature)
      }

      if (signature != "!") {
        val (className, addedFields, removedFields) = parseScalaTypeSignature(signature)
        apiDocValidation.validateDataTypeFields(className, dataTypeName, fieldNames, addedFields, removedFields)
      }

      DataType(dataTypeName, parameters)
    }

    def replace_leading_underscores(line: String): String =
      if (line == "")
        ""
      else if (line(0)=='_')
        "&nbsp;" + replace_leading_underscores(line.drop(1))
      else
        line

    def getApidoc(apiDocValidation: ApiDocValidation): ApiDocElement = {
      if (key.startsWith("GET ") || key.startsWith("POST ") || key.startsWith("PUT ") || key.startsWith("DELETE ") || key.startsWith("PATCH ") || key.startsWith("OPTIONS ")) {

        if (elements.nonEmpty)
          throw new Exception(s"""Elements for "$key" are not empty: $elements""")

        val pos = key.indexOf(' ')
        val uri = key.drop(pos).trim

        MethodAndUri(
          method = key.substring(0,pos).trim,
          uri = uri,
          uriParms = findUriParms(uri)
        )
      }

      else if (key=="DESCRIPTION")
        Description(
          shortDescription = elements.head,
          longDescription  = if (elements.length==1) None else Some(elements.tail.map(replace_leading_underscores).mkString("<br>"))
        )

      else if (key=="PARAMETERS")
        getParameters

      else if (key=="ERRORS")
        Errors(elements.map(element =>
          Error(
            code = element.substring(0,4).trim.toInt,
            message = element.drop(4).trim
          )
        ))

      else if (key=="RESULT") {
        getResults

      } else if (key.contains(":"))
        parseDataType(apiDocValidation, key)

      else
        throw new Exception(s"""Unknown key: "$key"""")
    }
  }

  private def parseRaw(
    lines: List[String],
    current: Raw,
    result: List[Raw],
    mainIndentLength: Int
  ): List[Raw] =

    if (lines.isEmpty)
      current::result

    else {
      val indentLength = getIndentLength(lines.head)

      val line = lines.head.trim

      if (indentLength < mainIndentLength)
        throw new Exception(s"""Bad indentation for line "$line"""")

      else if (indentLength > mainIndentLength)
        parseRaw(lines.tail, current.plus(line), result, mainIndentLength)

      else
        parseRaw(lines.tail, Raw(line, List()), current::result, mainIndentLength)
    }

  private def parseRaw(apidoc: String): List[Raw] = {
    val lines =
      apidoc.split("\n")
        .filter(line => line.trim.length>0)
        .toList // All non-empty lines.

    if(lines.nonEmpty) {
      val indentLength = getIndentLength(lines.head)
      val line = lines.head.trim
      parseRaw(lines.tail, Raw(line, List()), List(), indentLength)
    }
    else
      List.empty[Raw]

  }
          
  /**
    * Internal function. Public because of testing.
    */
  def getRaw(apidoc: String): JObject = {
    val raws = parseRaw(apidoc)
    JObject(raws.map(raw => raw.key -> JArray(raw.elements.map(JString))))
  }


  private def findElementOfType[T: ClassTag](elements: List[ApiDocElement], name: String, apidocString: String): T =
    if (elements.isEmpty)
      throw new Exception("Missing "+name+" in "+apidocString)
    else
      elements.head match {
        case a: T => a
        case _ => findElementOfType[T](elements.tail, name, apidocString)
      }

  private def maybeFindElementOfType[T: ClassTag](elements: List[ApiDocElement]): Option[T] =
    if (elements.isEmpty)
      None
    else
      elements.head match {
        case a: T => Some(a)
        case _ => maybeFindElementOfType[T](elements.tail)
      }

  def getApiDoc(apiDocValidation: ApiDocValidation, apidocString: String) = {
    val elements = parseRaw(apidocString).map(_.getApidoc(apiDocValidation))
    ApiDoc(
      findElementOfType[MethodAndUri](elements, "method and URI", apidocString),
      findElementOfType[Description](elements, "DESCRIPTION", apidocString),
      maybeFindElementOfType[Parameters](elements),
      maybeFindElementOfType[Errors](elements),
      maybeFindElementOfType[Results](elements)
    )
  }

  /**
    * Implementation:
    * {{{
  def getApiDocs(apidocStrings: List[String]) =
    ApiDocs(apidocStrings.map(getApiDoc))
    * }}}
    */
  def getApiDocs(apiDocValidation: ApiDocValidation, apidocStrings: List[String]) =
    ApiDocs(apidocStrings.map(string => getApiDoc(apiDocValidation, string)))

  def getDataTypes(apiDocValidation: ApiDocValidation, apidocStrings: List[String]) = {
    val elements = parseRaw(apidocStrings.mkString("\n")).map(_.getApidoc(apiDocValidation))
    DataTypes(elements.filter(_.isInstanceOf[DataType]).map(_.asInstanceOf[DataType]))
  }
}
