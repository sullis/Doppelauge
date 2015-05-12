name := """api_doc"""

version := "1.0"

scalaVersion := "2.11.1"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value

//libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"

scalacOptions in (Compile,doc) := Seq("-groups", "-implicits")
