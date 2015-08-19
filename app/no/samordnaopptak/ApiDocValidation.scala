package no.samordnaopptak.apidoc

import com.fasterxml.jackson.annotation.JsonIgnore

import play.api.Play.current

import no.samordnaopptak.json._


object ApiDocValidation{

  class MismatchPathParametersException(message: String) extends Exception(message)
  class MismatchFieldException(message: String) extends Exception(message)
  class AlreadyDefinedFieldException(message: String) extends Exception(message)
  class UnknownFieldException(message: String) extends Exception(message)

  private def safeLoadClass(className: String): java.lang.Class[_] =
    try{
      play.api.Play.classloader.loadClass(className)
     } catch {
        case e: java.lang.ClassNotFoundException => null.asInstanceOf[java.lang.Class[_]]
    }
 
  /*

  null, List("lib", "ApiDoc", "MismatchFieldException") -> loadClass(
                                                              null,
                                                              List("lib.ApiDoc"), "MismatchFieldException"
                                                           )

  null, List("lib.ApiDoc", "MismatchFieldException")    -> loadClass(
                                                              play.api.Play.classloader.loadClass("lib.ApiDoc"),
                                                              List("MismatchFieldException")
                                                           )

  Class("lib.ApiDoc"), List("MismatchFieldException")   -> parent.getClasses.find(_.getCannonicalName()=="lib.ApiDoc.MimatchFieldException").get

   */

  private def loadInnerClass(parent: java.lang.Class[_], className: String, elms: List[String]): java.lang.Class[_] = {
    //println(className+": "+"parent: "+(if (parent==null) "null" else parent.getCanonicalName())+", elms: "+elms)

    if (parent==null && elms.isEmpty) {
      play.api.Play.classloader.loadClass(className)

    } else if (parent==null) {
      val class_ = safeLoadClass(elms(0))
      if (class_ != null)
        loadInnerClass(class_, className, elms.tail)
      else if (elms.size==1)
        play.api.Play.classloader.loadClass(className)
      else
        loadInnerClass(null, className, elms(0)+"."+elms(1) :: elms.tail.tail)

    } else if (elms.isEmpty) {
      parent

    } else {
      val class_ = parent.getClasses.find(_.getCanonicalName()==parent.getCanonicalName()+"."+elms(0))
      if (class_ != None)
        loadInnerClass(class_.get, className, elms.tail)
      else
        play.api.Play.classloader.loadClass(className)
    }
  }

  def loadInnerClass(className: String): java.lang.Class[_] =
    loadInnerClass(null.asInstanceOf[java.lang.Class[_]], className, className.split('.').toList)


  def validateDataTypeFields(className: String, dataTypeName: String, fields: Set[String], addedFields: Set[String], removedFields: Set[String]): Unit = {

    val class_ = try{
      loadInnerClass(className)
    } catch {
      case e: java.lang.ClassNotFoundException =>
        //println("couldn't load class "+className+". Trying models instead")
        loadInnerClass("models."+className)
    }

    def getClassFieldNames(parameterAnnotations: List[Array[java.lang.annotation.Annotation]], fields: List[java.lang.reflect.Field]): List[String] = {
      if (fields.isEmpty)
        List()
      else {
        val field = fields.head
        val annotations = if (parameterAnnotations.isEmpty) field.getDeclaredAnnotations().toList else parameterAnnotations.head.toList
        val rest = getClassFieldNames(parameterAnnotations.drop(1), fields.tail) // List().drop(1) == List()

        val fieldName = field.getName()
        if(fieldName.contains("$"))
          rest
        else
          annotations.find(_.isInstanceOf[JsonIgnore]) match {
            case Some(annotation: JsonIgnore) if annotation.value==true => rest
            case None => fieldName :: rest
          }
      }
    }

    if(class_.getConstructors.isEmpty)
      throw new Exception(s"""While evaluating "${dataTypeName}": Class $class_ does not have any constructors.""")

    val classFields = getClassFieldNames(
      class_.getConstructors.head.getParameterAnnotations().toList,
      class_.getDeclaredFields().toList
    ).toSet

    if ( (removedFields &~ classFields).size > 0)
      throw new UnknownFieldException(s"""While evaluating "${dataTypeName}": One or more removedFields are not defined for class '$className': """ + (removedFields &~ classFields) + ". classFields: "+classFields)

    if ( (addedFields & classFields).size > 0)
      throw new AlreadyDefinedFieldException(s"""While evaluating "${dataTypeName}": One or more addedFields are already defined for class '$className': """+(addedFields & classFields))

    if ( (addedFields & removedFields).size > 0)
      throw new AlreadyDefinedFieldException(s"""While evaluating "${dataTypeName}": One or more fields are both present in addedFields and removedFields (for '$className'): """+(addedFields & removedFields))

    val modifiedClassFields = classFields ++ addedFields -- removedFields

    if ( fields != modifiedClassFields)
      throw new MismatchFieldException(s"""While evaluating "${dataTypeName}": The ApiDoc datatype does not match the class '$className'. Mismatched fields: """+ ((fields | modifiedClassFields) -- (fields & modifiedClassFields)))
  }

  def validate(apiDocs: ApiDocs) = {
    val uriParms = apiDocs.methodAndUri.uriParms
    val parameters: List[Variable] = apiDocs.parameters match{
      case None => List()
      case Some(parameters) => parameters.parameters
    }
    val pathParms = parameters.filter(_.paramType == ParamType.path)
    val pathParmKeys = pathParms.map(_.name)

    if (uriParms.size != pathParmKeys.size)
      throw new MismatchPathParametersException(s"""Mismatch between the number of parameters in the uri, and the number of path parameters.\nuriParms: $uriParms\npathParms:$pathParmKeys\njson: ${apiDocs.toJson}).""")

    pathParmKeys.foreach(pathParm =>
      if (!uriParms.contains(pathParm))
        throw new MismatchPathParametersException(s"""The path parameter "${pathParm}" is not defined in the path.""")
    )
  }

}
