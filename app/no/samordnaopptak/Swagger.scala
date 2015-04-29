package no.samordnaopptak.apidoc

import play.api.libs.json._

import TestByAnnotation.Test
import JsonUtil.Json



object SwaggerUtil{

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
    }
    if (format=="")
      Json.obj(
        "type" -> type_
      )
    else
      Json.obj(
        "type" -> type_,
        "format" -> format
      )
  }

  val header = Json.obj(
    "swagger" -> "2.0",
    //"host": "api.uber.com",
    /*
    "schemes" -> Json.arr(
        "https"
    ),
     */
    //"basePath" -> "",
    "produces" -> Json.arr(
      "application/json"
    )
  )


  private def jsonWithoutUndefined(json: JsObject) = JsObject(
    json.value.toSeq.filter {
      case (name, attributes) => ! attributes.isInstanceOf[JsUndefined]
    }
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
  def getTag(basePath: String, uri_raw: String): String = {

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

  private def getType(json: Json, addDescription: Boolean = true) = {
    val type_      = json("type").asString
    val isAtomType = ApiDocUtil.atomTypes.contains(type_)
    val isArray    = json("isArray").asBoolean
    val isEnum     = json("isEnum").asBoolean
    val type2 =
      if (isAtomType)
        atomTypeToSwaggerType(type_)
      else
        Json.obj(
          "$ref" -> ("#/definitions/" + type_)
        )

    val description =
      if (json.hasKey("noComment") || !json.hasKey("comment") || addDescription==false)
        Json.obj()
      else
        Json.obj(
          "description" -> json("comment").asString
        )

    if (isArray)
      Json.obj(
        "type" -> "array",
        "items" -> type2
      ) ++ description
    else if (isEnum)
      type2 ++
      Json.obj(
        "enum" -> JsArray(json("enumArgs").asList.map(j => JsString(j.asString)))
      ) ++
      description
    else
      type2 ++ description
  }

  private def getResponseErrors(errors: Json) = {
    //println("errors: "+errors)
    val code = errors("code").asNumber.toString
    val message = errors("message").asString
    Json.obj(
      code -> Json.obj(
        "description" -> message
      )
    )
  }

  private def getResult(result: Json) =
    Json.obj(
      "200" -> (
        Json.obj(
          "description" -> result("comment").asString,
          "schema" -> getType(result, addDescription=false)
        )
      )
    )

  private def getResponses(apidoc: Json): JsObject = {
    val errorAsJson =
      if (apidoc.hasKey("errors"))
        JsonUtil.flattenJsObjects(
          apidoc("errors").asList.map( error =>
            getResponseErrors(error)
          )
        )
      else
        Json.obj()

    val resultAsJson =
      if (apidoc.hasKey("result"))
        getResult(apidoc("result"))
      else
        Json.obj()

    errorAsJson ++ resultAsJson
  }

  def getApi(basePath: String, apidoc: Json): JsObject = {
    val method = apidoc("method").asString.toLowerCase
    Json.obj(
      method -> Json.obj(
        "summary" -> apidoc("shortDescription").asString,
        "description" -> apidoc("longDescription").asString,
        "parameters" -> JsArray(
          if (apidoc.hasKey("parameters"))
            (apidoc("parameters").asMap.map {
              case (name: String, attributes: Json) => jsonWithoutUndefined(
                Json.obj(
                  "name" -> name,
                  "in" -> attributes("paramType").asString,
                  "required" -> attributes("required").asBoolean,
                  "description" -> (if (attributes.hasKey("noComment")) "" else attributes("comment").asString)
                ) ++ (
                  if (attributes("isEnum").asBoolean==true)
                    getType(attributes, addDescription=false)
                  else
                    Json.obj(
                      "schema" -> getType(attributes, addDescription=false)
                    )
                )
              )
            }).toList
          else
            List()
        ),
        "tags" -> Json.arr(createTagName(getTag(basePath, apidoc("uri").asString))),
        "responses" -> getResponses(apidoc)
      )
    )
  }

  private def getDefinition(dataType: Json): JsObject =
    JsObject(
      dataType.asMap.map {
        case (name: String, attributes: Json) => {
          val required = attributes.asMap.filter {
            case (_, attributes) => attributes("required").asBoolean
          }.map{
            case (name, _) => name
          }
          name -> Json.obj(
            "id" -> name,
            "required" -> required,
            "properties" -> JsObject(
              attributes.asMap.map {
                case (name, attributes) =>
                  name -> jsonWithoutUndefined(
                    getType(attributes)
                  )
              }.toList
            )
          )
        }
      }.toList
    )

  private def getDefinitions(dataTypes: List[Json]): JsObject =
    JsonUtil.flattenJsObjects(dataTypes.map(getDefinition(_)))

  @Test(code="""
      self.allTags("/api/v1/", List()) === Set()
      self.allTags("/api/v1/", test.lib.ApiDocSamples.allUsers) === Set("users", "usernames")
      self.allTags("/api/v1/", test.lib.ApiDocSamples.allAcls)  === Set("acl")
      self.allTags("/api/v1/", test.lib.ApiDocSamples.all)      === Set("acl", "usernames", "users")
  """)
  def allTags(basePath: String, apidocs: List[String]): Set[String] = {
    val ret = apidocs.map(
      ApiDocUtil.getJson(_)
    ).map(jsonApiDoc =>
      (jsonApiDoc \ "uri").as[String]
    ).map(
      getTag(basePath, _)
    ).toSet

    ret
  }

  private def validateUniqueDataTypes(dataTypes: List[Json]) = {
    val names = dataTypes.flatMap(_.asMap.keys)
    if (names.size != names.distinct.size)
      throw new Exception("One or more ApiDoc datatypes defined more than once: "+names.diff(names.distinct).take(4))
  }


  // {User -> {id -> {type -> String}}} -> Set(String)
  private def getUsedDataTypesInDataType(dataType: Json): Set[String] = {
    val dataTypeValues = dataType.asMap.values
    val attributeValues = dataTypeValues.flatMap(_.asMap.values)
    //println("attributeValues: "+attributeValues.map(_("type").asString).toSet)
    attributeValues.map(_("type").asString).toSet
  }

  private def getUsedDatatypesInDatatypes(dataTypes: List[Json]): Set[String] = {
    if (dataTypes.isEmpty)
      Set()
    else {
      val dataType = dataTypes.head
      val subTypes = getUsedDataTypesInDataType(dataType)
      subTypes ++ getUsedDatatypesInDatatypes(dataTypes.tail)
    }
  }

  private def getUsedDatatypesInJson(jsonApiDocs: List[Json]): Set[String] = {
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

  private def validateThatAllDatatypesAreDefined(tag: String, jsonApiDocs: List[Json], dataTypes: List[Json]): Unit = {
    val definedTypes: Set[String] = dataTypes.flatMap(_.asMap.keys).toSet ++ ApiDocUtil.atomTypes
    val usedTypes: Set[String]    = getUsedDatatypesInDatatypes(dataTypes) ++ getUsedDatatypesInJson(jsonApiDocs)
    val undefinedTypes            = usedTypes -- definedTypes
    /*
    println("definedTypes: "+definedTypes)
    println("usedTypes: "+usedTypes)
     */
    if (undefinedTypes.size>0)
      throw new Exception(s"""${undefinedTypes.size} ApiDoc datatype(s) was/were undefined while evaluating "$tag": """+undefinedTypes.toList.sorted.map(s => s""""$s"""").toString.drop(4))
  }

  def getEndpoint(basePath: String, path: String, apidocs: List[Json]): JsObject = {
    val relevantApiDocs = apidocs.filter(_("uri").asString==path)

    JsonUtil.flattenJsObjects(relevantApiDocs.map(getApi(basePath, _)))
  }

  private def getAllPaths(apidocs: List[Json]): Set[String] =
    apidocs.map(_("uri").asString).toSet

  def getMain(basePath: String, apidocs: List[String]): JsObject = {

    if (!basePath.endsWith("/"))
      throw new Exception("Basepath must end with slash: "+basePath)

    val dataTypes = apidocs.map(apidoc => JsonUtil.jsValue(ApiDocUtil.getDataTypes(apidoc)))
    validateUniqueDataTypes(dataTypes)

    val tags = allTags(basePath, apidocs).toList.sorted

    val jsonApiDocs = apidocs.map(a => JsonUtil.jsValue(ApiDocUtil.getJson(a)))

    validateThatAllDatatypesAreDefined(basePath, jsonApiDocs, dataTypes)

    val allPaths = getAllPaths(jsonApiDocs)

    val groupedJsonApiDocs = jsonApiDocs.groupBy(_("uri").asString)

    val groupedJsonApiDocsAsJsValue = (groupedJsonApiDocs map {
      case (key, jsonApiDoc) => Json.obj(
        key -> getEndpoint(basePath, key, jsonApiDoc)
      )
    }).toList

    //println("jsonapidocs: "+groupedJsonApiDocs)

    header ++
    Json.obj(
      "paths" -> JsonUtil.flattenJsObjects(groupedJsonApiDocsAsJsValue),
      "definitions" -> getDefinitions(dataTypes)
    )
  }
}
