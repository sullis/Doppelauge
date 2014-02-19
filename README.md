
* To publish, write "publishSigned" in play console.
* To select which files to publish, edit ApplicationBuild.main in project/Build.scala


Example:


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
      firstName: String  <- Attribute comment
      properties: Properties

    Properties:
      id: String



![Build Status](https://magnum.travis-ci.com/sun-opsys/api-doc.png?token=???&branch=master)

https://magnum.travis-ci.com/sun-opsys/api-doc
