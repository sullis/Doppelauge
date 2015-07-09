


API-doc
=========

Non-cluttering swagger doc for scala playframework.

This API-doc does the following validation checks:

  * All endpoints must be documented
  * The endpoints in the api docs must match the endpoints in conf/routes
  * All used datatypes must be defined
  * Datatypes must have corresponding scala classes.
  * The fields in datatypes must be the same as the fields in the corresponding scala classes.
  * Datatypes can only be defined once



Play 2.3 vs. Play 2.4
=====================

The master branch uses Play 2.4
Play 2.3 is supported in the play_2.3 branch.



QUICK START
===========

1. execute "activator run" in the terminal.

2. Go to this address in your browser:

   http://localhost:9000/swagger-ui/dist/index.html?url=http://localhost:9000/api/v1/api-docs



Examples
==========

Basic usage:


    @no.samordnaopptak.apidoc.ApiDoc(doc="""
      GET /api/v1/users/{id}

      DESCRIPTION
        Get user

      PARAMETERS 
        id: String <- The user id
        names: Array String <- All names in an array of strings
        type: Enum(Woman, Man, Dog) String <- The user can either be a woman, a man, or a dog.

      ERRORS
        400 User not found
        400 Syntax Error
    """)
    def getUser(id: String) = ...



Define parameter types:

    @no.samordnaopptak.apidoc.ApiDoc(doc="""
      GET /api/v1/users/{phone}

      DESCRIPTION
        Get user

      PARAMETERS 
        id: String(path)                 <- 'path' parameter type (default, unless the parameter name is 'body')
        body: String                     <- 'body' parameter type.
        phone: String(query)             <- 'query' parameter type
        mobile: String(query, optional)  <- Optional 'query' parameter type
        email: String(form)              <- 'form' parameter type
        age: Enum(10,20,30,40) Int       <- Enum Int parameter.
    """)
    def getUser(id: String) = ...



Defining types:

    case class User(id: String, firstName: String)

    @no.samordnaopptak.apidoc.ApiDoc(doc="""
      GET /api/v1/users/{id}

      DESCRIPTION
        Get user

      PARAMETERS 
        id: String <- Parameter comment

      ERRORS
        400 User not found
        400 Syntax Error

      RESULT
        User

      User:
        id: String
        firstName: String  <- The first name of the user.
        age: Enum(10,20,30,40) Int <- Age can be either 10, 20, 30 or 40
    """)
    def getUser(id: String) = ...





Defining types, based on an almost matching existing class:

    case class User(id: String, firstName: String)

    @no.samordnaopptak.apidoc.ApiDoc(doc="""
      GET /api/v1/users/{id}

      DESCRIPTION
        Get user

      PARAMETERS 
        id: String <- Parameter comment

      ERRORS
        400 User not found
        400 Syntax Error

      RESULT
        User

      User(-firstName, +lastName):
        id: String
        lastName: String
    """)
    def getUser(id: String) = ...




Defining types, based on an almost matching existing class, using @JsonIgnore:

    import com.fasterxml.jackson.annotation._
    case class User(id: String, @JsonIgnore firstName: String)

    @no.samordnaopptak.apidoc.ApiDoc(doc="""
      GET /api/v1/users/{id}

      DESCRIPTION
        Get user

      PARAMETERS 
        id: String <- Parameter comment

      ERRORS
        400 User not found
        400 Syntax Error

      RESULT
        User

      User(+lastName):
        id: String
        lastName: String
    """)
    def getUser(id: String) = ...




Defining types, based on a class defined elsewhere:

    @no.samordnaopptak.apidoc.ApiDoc(doc="""
      GET /api/v1/users/{id}

      DESCRIPTION
        Get user

      PARAMETERS 
        id: String <- Parameter comment

      ERRORS
        400 User not found
        400 Syntax Error

      RESULT
        User

      User: models.user.User(-firstName, +lastName)
        id: String
        lastName: String
    """)
    def getUser(id: String) = ...




Defining types, not based on a class:

    @no.samordnaopptak.apidoc.ApiDoc(doc="""
      GET /api/v1/users/{id}

      DESCRIPTION
        Get user

      PARAMETERS 
        id: String <- Parameter comment

      ERRORS
        400 User not found
        400 Syntax Error

      RESULT
        User

      User: !
        id: String
        lastName: String
    """)
    def getUser(id: String) = ...



Defining a type with unknown number of elements:

    @no.samordnaopptak.apidoc.ApiDoc(doc="""
      GET /api/v1/users/{id}

      DESCRIPTION
        Get user

      PARAMETERS 
        id: String <- Parameter comment

      ERRORS
        400 User not found
        400 Syntax Error

      RESULT
        User

      User: !
        id: String
        lastName: String
        ...
    """)
    def getUser(id: String) = ...




Notes
=========

* Scala classes must be specified with full path. (I.e. you have to write "controllers.User", not "User".)
  The only exception is if the classes is in the "models" package. (I.e. it is possible to write "User" instead of "models.User").

* In order to run validation checks on the api docs, it is necessary to call `ApiDocController.validate("/")`.
  If using specs2, it might be a good idea to put the following code (or something similar) into tests/ApiDocControllerSpec.scala:

  ```scala
    package test

    import org.specs2.mutable.Specification
    import play.api.test._
    import play.api.test.Helpers._


    class ApiDocControllerSpec extends Specification {
      "Validate swagger api docs" in {
          controllers.ApiDocController.validate()
      }
    }
  ```

  If there are end points defined in the "routes" file which have no api doc, you need to remove these before calling validate():

  ```scala
    class ApiDocControllerSpec extends Specification {
      "Validate swagger api docs" in {
          val routeEntries =
             no.samordnaopptak.apidoc.RoutesHelper.getRouteEntries()
               .filter(_.scalaClass != "controllers.Assets") // no api-doc for the static assets files
          controllers.ApiDocController.validate(routeEntries)
      }
    }
  ```


License
==========

Copyright 2013-2014 SUN/OPSYS at University of Oslo.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at [apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0)

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
