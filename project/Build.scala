import sbt._
import Keys._
import play.sbt.PlayScala

object ApplicationBuild extends Build {

  val appName         = "doppelauge"
  val appVersion      = "%s".format("git describe --tags --long --always".!!.trim)

  val appDependencies = Seq()

  val main = Project(appName, file(".")).enablePlugins(PlayScala).settings(
    version := appVersion,
    libraryDependencies ++= appDependencies,

    // Add your own project settings here
    organization := "no.samordnaopptak",

    publishMavenStyle := true,

/*    

    publishTo := {
      val nexus = "https://repo.usit.uio.no/nexus/"
      if (isSnapshot.value)
        Some("snapshots" at nexus + "content/repositories/opsys-internal-snapshots")
      else
        Some("releases"  at nexus + "content/repositories/opsys-internal-release")
    },
 */

    mappings in (Compile, packageBin) ~= { (ms: Seq[(File, String)]) =>
      ms filter {
        case (file, toPath) => {
          //println("file: "+file+"\ntoPath: "+toPath+"\n")
          val doit = toPath.startsWith("no/samordnaopptak") || toPath.contains("swagger-ui")
          if (doit){
            println("file: "+file+"\ntoPath: "+toPath+"\n")
          }
          doit
        }
      }
    }

  )

}
