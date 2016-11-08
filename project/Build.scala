import sbt._
import Keys._
import com.typesafe.config._
import play.sbt.PlayScala
import java.io.PrintWriter

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

//    routesGenerator := InjectedRoutesGenerator,

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

  def writeToFile(fileName: String, value: String) = {
    val file = new PrintWriter(new java.io.File(fileName))
    try { file.print(value+"\n")
    } finally { file.close() }
  }
  def formatForConf(key: String, value: String): String = {
    val ret = key+"=\""+value+'"'
    //val ret = s"""$key="$value""""
    ret
  }

  val conf = ConfigFactory.parseFile(new File("conf/application.conf")).resolve()
  val _appVersionFilename = conf.getString("application.version.file")
  writeToFile(_appVersionFilename, formatForConf("current", appVersion))

}
