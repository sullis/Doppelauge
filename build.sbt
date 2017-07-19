name := """doppelauge"""

version := "1.5.0"

scalaVersion := "2.12.2"

crossScalaVersions := Seq("2.11.11", "2.12.2")

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value

libraryDependencies += "org.specs2" %% "specs2-core" % "3.9.1" % "test"

scalacOptions in (Compile,doc) := Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-groups",
  "-implicits",
  "-feature",
  "-Xlint",
  "-Xfatal-warnings",
  "-Ywarn-unused-import",
  "-Ywarn-numeric-widen",
  "-Ywarn-dead-code"
)

resolvers += "scalaz-bintray" at "https://dl.bintray.com/scalaz/releases"

// Don't generate scaladoc for the "controller" and "router" packages
scalacOptions in (Compile, doc) := List(
  "-skip-packages",  "controllers:router",
  "-doc-source-url", "https://github.com/sun-opsys/Doppelauge/tree/master€{FILE_PATH}.scala"
)

