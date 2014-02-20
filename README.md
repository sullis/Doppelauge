
API-doc
=========

Non-cluttering swagger doc for scala playframework.



QUICK START
===========

1. execute "play run" in the terminal.

2. Run your local copy of swagger, and point your browser there.

3. Insert http://localhost:9000/api-docs into swagger.



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




License
-------

Copyright 2013-2014 SUN/OPSYS at University of Oslo.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at [apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0)

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
