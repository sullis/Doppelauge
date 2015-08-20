package no.samordnaopptak.apidoc

import no.samordnaopptak.test.TestByAnnotation.Test
import no.samordnaopptak.json._

/*
 Generates swagger 2.0 data
 */

object SwaggerUtil{

  val atomTypes = Set("etc.", "String", "Long", "Boolean", "Integer", "Int", "Any", "Double", "Float", "Date", "DateTime")


  // https://github.com/swagger-api/swagger-core/wiki/Datatypes
  def atomTypeToSwaggerType(atomType: String) = {
    val (type_,format) = atomType match {
      case "etc." => ("etc.","")
      case "String" => ("string","")
      case "Long" => ("integer","int64")
      case "Boolean" => ("boolean","")
      case "Integer" => ("integer","int32")
      case "Int" => ("integer","int32")
      case "Any" => ("any","")
      case "Double" => ("number","double")
      case "Float" => ("number","float")
      case "Date" => ("string", "date")
      case "DateTime" => ("string", "date-time")
    }
    if (format=="")
      J.obj(
        "type" -> type_
      )
    else
      J.obj(
        "type" -> type_,
        "format" -> format
      )
  }

  val header = J.obj(
    "swagger" -> "2.0",
    //"host": "api.uber.com",
    /*
    "schemes" -> J.arr(
        "https"
    ),
     */
    //"basePath" -> "",
    "produces" -> J.arr(
      "application/json"
    )
  )


  @Test(code="""
     self.getTag("/api/v1/", "/api/v1/users/habla/happ") === "users"
     self.getTag("/api/v1/", "/api/v1/users/habla/")     === "users"
     self.getTag("/api/v1/", "/api/v1/users/habla/{id}") === "users"
     self.getTag("/api/v1/", "/api/v1/users/habla")      === "users"
     self.getTag("/api/v1/", "/api/v1/users/{id}")       === "users"
     self.getTag("/api/v1/", "/api/v1/users/")           === "users"
     self.getTag("/api/v1/", "/api/v1/users")            === "users"
     self.getTag("/api/v1/", "/users")                   === "users"
     self.getTag("/api/v1/", "/users_and_houses")        === "users_and_houses"
     self.getTag("/api/v1/", "/gakk/users_and_houses")   === "gakk"
     self.getTag("/api/v1/", "/a/b")                     === "a"
     self.getTag("/api/v1/", "/api/v1")                  === "root"
     self.getTag("/api/v1/", "/api/v1/")                 === "root"
  """)
  private def getTag(basePath: String, uri_raw: String): String = {

    assert(uri_raw.startsWith("/"))

    val uri =
      if (uri_raw.endsWith("/"))
        uri_raw.dropRight(1)
      else
        uri_raw

    if (basePath == uri+"/")
      "root"

    else if (!uri.startsWith(basePath))
      getTag("/", uri)

    else {

      val fullPathTail = uri.drop(basePath.length)
      val firstSlash = fullPathTail.indexOf('/')
      if (firstSlash == -1)
        fullPathTail
      else
        fullPathTail.take(firstSlash)

    }

  }

  private def createTagName(tag: String) =
    tag(0).toUpper + tag.tail

  private def getType(json: JValue, addDescription: Boolean = true) = {
    val type_      = json("type").asString
    val isAtomType = atomTypes.contains(type_)
    val isArray    = json("isArray").asBoolean
    val isEnum     = json("isEnum").asBoolean
    val type2 =
      if (isAtomType)
        atomTypeToSwaggerType(type_)
      else
        J.obj(
          "$ref" -> ("#/definitions/" + type_)
        )

    val description =
      if (json.hasKey("noComment") || !json.hasKey("comment") || addDescription==false)
        J.obj()
      else
        J.obj(
          "description" -> json("comment").asString
        )

    if (isArray)
      J.obj(
        "type" -> "array",
        "items" -> type2
      ) ++ description
    else if (isEnum)
      type2 ++
      J.obj(
        "enum" -> json("enumArgs")
      ) ++
      description
    else
      type2 ++ description
  }

  private def getTypeFromField(field: Field) = {
    val isAtomType = atomTypes.contains(field.type_)

    val type2 =
      if (isAtomType)
        atomTypeToSwaggerType(field.type_)
      else
        J.obj(
          "$ref" -> ("#/definitions/" + field.type_)
        )

    val description =
      field.comment match {
        case None => J.obj()
        case Some(comment) => J.obj("description" -> comment)
      }

    if (field.isArray)
      J.obj(
        "type" -> "array",
        "items" -> type2
      ) ++
      description

    else if (field.isEnum)
      type2 ++
      J.obj(
        "enum" -> field.enumArgs
      ) ++
      description

    else
      type2 ++ description
  }

  private def getResponseErrors(errors: JValue) = {
    //println("errors: "+errors)
    val code = errors("code").asNumber.toString
    val message = errors("message").asString
    J.obj(
      code -> J.obj(
        "description" -> message
      )
    )
  }

  private def getResult(result: JValue) =
    J.obj(
      "200" -> (
        J.obj(
          "description" -> result("comment").asString,
          "schema" -> getType(result, addDescription=false)
        )
      )
    )

  private def getResponses(apidoc: JValue): JObject = {
    val errorAsJson =
      if (apidoc.hasKey("errors"))
        J.flattenJObjects(
          apidoc("errors").asArray.map( error =>
            getResponseErrors(error)
          )
        )
      else
        J.obj()

    val resultAsJson =
      if (apidoc.hasKey("result"))
        getResult(apidoc("result"))
      else
        J.obj()

    errorAsJson ++ resultAsJson
  }

  def getApi(basePath: String, apidoc: JValue): JObject = {
    val method = apidoc("method").asString.toLowerCase
    J.obj(
      method -> J.obj(
        "summary" -> apidoc("shortDescription").asString,
        "description" -> apidoc("longDescription").asString,
        "parameters" -> JArray(
          if (apidoc.hasKey("parameters"))
            (apidoc("parameters").asMap.map {
              case (name: String, attributes: JValue) =>
                val in = attributes("paramType").asString
                J.obj(
                  "name" -> name,
                  "in" -> in,
                  "required" -> attributes("required").asBoolean,
                  "description" -> (if (attributes.hasKey("noComment")) "" else attributes("comment").asString)
                ) ++ (
                  if (in != "body")
                    getType(attributes, addDescription=false)
                  else
                    J.obj(
                      "schema" -> getType(attributes, addDescription=false)
                    )
                )
            }).toList
          else
            List()
        ),
        "tags" -> J.arr(createTagName(getTag(basePath, apidoc("uri").asString))),
        "responses" -> getResponses(apidoc)
      )
    )
  }

  private def getDefinition(dataType: DataType): JObject = {
    val fields = dataType.parameters.fields

    J.obj(
      dataType.name -> J.obj(
        "required"   -> fields.filter(_.required).map(_.name),
        "properties" -> fields.map( field =>
          field.name -> getTypeFromField(field)
        ).toMap
      )
    )
  }

  private def getDefinitions(dataTypes: DataTypes): JObject =
    J.flattenJObjects(dataTypes.dataTypes.map(getDefinition(_)))

  @Test(code="""
      self.allTags("/api/v1/", List()) === Set()
      self.allTags("/api/v1/", test.lib.ApiDocSamples.allUsers) === Set("users", "usernames")
      self.allTags("/api/v1/", test.lib.ApiDocSamples.allAcls)  === Set("acl")
      self.allTags("/api/v1/", test.lib.ApiDocSamples.all)      === Set("acl", "usernames", "users")
  """)
  private def allTags(basePath: String, apidocstrings: List[String]): Set[String] = {
    val ret = apidocstrings.map(
      apidocstring => ApiDocParser.getJson(apidocstring)
    ).map(jsonApiDoc =>
      jsonApiDoc("uri").asString
    ).map(
      getTag(basePath, _)
    ).toSet

    ret
  }

  private def getUsedDatatypesInJson(jsonApiDocs: List[JValue]): Set[String] = {
    if (jsonApiDocs.isEmpty)
      Set()
    else {
      val json = jsonApiDocs.head
      val parameterTypes = if (json.hasKey("parameters"))
                              json("parameters").asMap.values.map(_("type").asString).toSet
                           else
                              Set()
      val returnType = if (json.hasKey("result"))
                         Set(json("result")("type").asString)
                       else
                         Set()
      /*
      println("json: "+json)
      println("parameterTypes: "+parameterTypes+", "+parameterTypes.size)
      println("returnType: "+returnType+", "+returnType.size)
       */
      parameterTypes ++ returnType ++ getUsedDatatypesInJson(jsonApiDocs.tail)
    }
  }

  private def validateThatAllDatatypesAreDefined(tag: String, jsonApiDocs: List[JValue], dataTypes: DataTypes): Unit = {
    val definedTypes: Set[String] = dataTypes.names.toSet ++ atomTypes
    val usedTypes: Set[String]    = dataTypes.usedDataTypes ++ getUsedDatatypesInJson(jsonApiDocs)
    val undefinedTypes            = usedTypes -- definedTypes
    /*
    println("definedTypes: "+definedTypes)
    println("usedTypes: "+usedTypes)
     */
    if (undefinedTypes.size>0)
      throw new Exception(s"""${undefinedTypes.size} ApiDoc datatype(s) was/were undefined while evaluating "$tag": """+undefinedTypes.toList.sorted.map(s => s""""$s"""").toString.drop(4))
  }

  def getEndpoint(basePath: String, path: String, apidocs: List[JValue]): JObject = {
    val relevantApiDocs = apidocs.filter(_("uri").asString==path)

    J.flattenJObjects(relevantApiDocs.map(getApi(basePath, _)))
  }

  private def getAllPaths(apidocs: List[JValue]): Set[String] =
    apidocs.map(_("uri").asString).toSet

  private def sortMapByKey[T](group: Map[String, T]): List[(String, T)] =
    group.toList.sortBy{
      case (key, _) => key
    }

  def getMain(basePath: String, apidocstrings: List[String]): JObject = {

    if (!basePath.endsWith("/"))
      throw new Exception("Basepath must end with slash: "+basePath)

    val dataTypes = ApiDocParser.getDataTypes(apidocstrings.mkString("\n"))

    val tags = allTags(basePath, apidocstrings).toList.sorted

    val jsonApiDocs = apidocstrings.map(a => ApiDocParser.getJson(a))

    validateThatAllDatatypesAreDefined(basePath, jsonApiDocs, dataTypes)

    val allPaths = getAllPaths(jsonApiDocs)

    val groupedJsonApiDocs = sortMapByKey(
      jsonApiDocs.groupBy(_("uri").asString)
    )

    val groupedJsonApiDocsAsJsValue = (groupedJsonApiDocs map {
      case (key, jsonApiDoc) => J.obj(
        key -> getEndpoint(basePath, key, jsonApiDoc)
      )
    }).toList

    //println("jsonapidocs: "+groupedJsonApiDocs)

    header ++
    J.obj(
      "paths" -> J.flattenJObjects(groupedJsonApiDocsAsJsValue),
      "definitions" -> getDefinitions(dataTypes)
    )
  }
}
