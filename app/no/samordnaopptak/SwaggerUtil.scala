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

  private def getTypeFromField(field: Field, addDescription: Boolean = true) = {
    val isAtomType = atomTypes.contains(field.type_)

    val type2 =
      if (isAtomType)
        atomTypeToSwaggerType(field.type_)
      else
        J.obj(
          "$ref" -> ("#/definitions/" + field.type_)
        )

    val description =
      if (addDescription)
        field.comment match {
          case None => J.obj()
          case Some(comment) => J.obj("description" -> comment)
        }
      else
        J.obj()

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

  private def getResponseError(error: Error) =
    J.obj(
      error.code.toString -> J.obj("description" -> error.message)
    )

  private def getResult(result: Result) =
    J.obj(
      result.code.toString -> (
        J.obj(
          "description" -> result.field.comment.getOrElse(""),
          "schema" -> getTypeFromField(result.field, addDescription=false)
        )
      )
    )

  private def getResults(results: Results) =
    J.flattenJObjects(results.results.map(getResult))

  private def getResponses(apidoc: ApiDocs): JObject = {
    val errorAsJson =
      apidoc.errors match {
        case None => J.obj()
        case Some(errors) => {
          val codes = errors.errors.map(_.code)
          if (codes.size != codes.toSet.size)
            throw new Exception("Error codes defined more than once for the api doc " + apidoc.toJson.pp())
          J.flattenJObjects(errors.errors.map(getResponseError))
        }
      }

    val resultAsJson =
      apidoc.results match {
        case None => J.obj()
        case Some(results) => getResults(results)
      }

    errorAsJson ++ resultAsJson
  }

  def getApi(basePath: String, apidoc: ApiDocs): JObject = {
    val method = apidoc.methodAndUri.method.toLowerCase
    J.obj(
      method -> J.obj(
        "summary" -> apidoc.description.shortDescription,
        "description" -> apidoc.description.longDescription.getOrElse(""),
        "parameters" -> (apidoc.parameters match {
          case None => J.arr()
          case Some(parameters) => JArray(
            parameters.fields.map(field => {
              val in = field.paramType.toString
              J.obj(
                "name" -> field.name,
                "in" -> in,
                "required" -> field.required,
                "description" -> field.comment.getOrElse("")
              ) ++ (
                if (in != "body")
                  getTypeFromField(field, addDescription=false)
                else
                  J.obj(
                    "schema" -> getTypeFromField(field, addDescription=false)
                  )
              )
            }).toList
          )
        }),
        "tags" -> J.arr(createTagName(getTag(basePath, apidoc.methodAndUri.uri))),
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
      self.allTags("/api/v1/", test.ApiDocSamples.allUsers) === Set("users", "usernames")
      self.allTags("/api/v1/", test.ApiDocSamples.allAcls)  === Set("acl")
      self.allTags("/api/v1/", test.ApiDocSamples.all)      === Set("acl", "usernames", "users")
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

  private def validateThatAllDatatypesAreDefined(basePath: String, apiDocs: List[ApiDocs], dataTypes: DataTypes): Unit = {
    val definedTypes: Set[String] = dataTypes.names.toSet ++ atomTypes
    val usedTypes: Set[String]    = dataTypes.usedDataTypes ++ apiDocs.flatMap(_.usedDataTypes)
    val undefinedTypes            = usedTypes -- definedTypes
    /*
    println("definedTypes: "+definedTypes)
    println("usedTypes: "+usedTypes)
     */
    if (undefinedTypes.size>0)
      throw new Exception(s"""${undefinedTypes.size} ApiDoc datatype(s) was/were undefined while evaluating "$basePath": """+undefinedTypes.toList.sorted.map(s => s""""$s"""").toString.drop(4))
  }

  def getEndpoint(basePath: String, path: String, apidocs: List[ApiDocs]): JObject = {
    val relevantApiDocs = apidocs.filter(_.methodAndUri.uri==path)

    J.flattenJObjects(relevantApiDocs.map(getApi(basePath, _)))
  }

  private def sortMapByKey[T](group: Map[String, T]): List[(String, T)] =
    group.toList.sortBy{
      case (key, _) => key
    }

  def getMain(basePath: String, apidocstrings: List[String]): JObject = {

    if (!basePath.endsWith("/"))
      throw new Exception("Basepath must end with slash: "+basePath)

    val dataTypes = ApiDocParser.getDataTypes(apidocstrings.mkString("\n"))

    val tags = allTags(basePath, apidocstrings).toList.sorted

    val apiDocs = apidocstrings.map(ApiDocParser.getApiDocs)

    validateThatAllDatatypesAreDefined(basePath, apiDocs, dataTypes)

    val groupedApiDocs = sortMapByKey(
      apiDocs.groupBy(_.methodAndUri.uri)
    )

    val groupedApiDocsAsJsValue = (groupedApiDocs map {
      case (key, apiDoc) => J.obj(
        key -> getEndpoint(basePath, key, apiDoc)
      )
    }).toList

    header ++
    J.obj(
      "paths" -> J.flattenJObjects(groupedApiDocsAsJsValue),
      "definitions" -> getDefinitions(dataTypes)
    )
  }
}
