
Doppelauge
==========

Doppelauge is a set of four libraries for the Play Framework.
All code should be thoroughly tested.

* Json library: [J](JSON.md#j)
* Json pattern matcher: [JsonMatcher](JSON.md#jsonmatcher)
* Swagger 2.0 Api doc generator: [API_DOC.md](API_DOC.md)
* Test by annotation test system: [TEST_BY_ANNOTATION.md](TEST_BY_ANNOTATION.md)



ScalaDoc
========
http://folk.uio.no/ksvalast/doppelauge/



Play 2.3 vs. Play 2.4
=====================

* The master branch uses Play 2.4.
* Play 2.3 is supported in the play_2.3 branch, but is less updated.



Installation
============
Add the following lines to build.sbt:

  ```
  resolvers += "jitpack" at "https://jitpack.io"
  libraryDependencies += "com.github.sun-opsys" % "doppelauge" % "1.0.0"
  ```


Credits
=======
The name "Doppelauge" was suggested by Hans Wilmers.


License
==========

Copyright 2013-2015 SUN/OPSYS at University of Oslo.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at [apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0)

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
