
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



QUICK START
===========

1. execute "play run" in the terminal.

2. Insert http://localhost:9000/api-docs into the swagger ui.



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
        id: String(path)   <- 'path' parameter type (default, unless the parameter name is 'body')
        body: String        <- 'body' parameter type.
        phone: String(query) <- 'query' parameter type
        email: String(form)   <- 'form' parameter type
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
    """)
    def getUser(id: String) = ...





Defining types, based on an existing class:

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

* In order to run validation checks on the api docs, it is necessary to call `ApiDocController.validate("/api/v1/")`.
  If using specs2, it might be a good idea to put the following code (or something similar) into tests/ApiDocControllerSpec.scala:

`

    package test

    import org.specs2.mutable.Specification
    import play.api.test._
    import play.api.test.Helpers._


    class ApiDocControllerSpec extends Specification {
      "Validate swagger api docs" in {
          controllers.ApiDocController.validate("/api/v1/")
      }
    }
`


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
