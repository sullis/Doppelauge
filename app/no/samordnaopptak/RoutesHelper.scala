package no.samordnaopptak.apidoc

import no.samordnaopptak.test.TestByAnnotation.Test


case class RouteEntry(restMethod: String, uri: String, scalaClass: String, scalaMethod: String) {

  @Test(code="""
    self.getDocUri2("/api/v1/acl/$service<[^/]+>") === "/api/v1/acl/{service}"
    self.getDocUri2("/api/v1/acl/$service<[asdf/e/aer/4343[]1534]>") === "/api/v1/acl/{service}"
    self.getDocUri2("/api/v1/acl/$service<>") === "/api/v1/acl/{service}"

    // one parameter in the middle of the uri:
    self.getDocUri2("/api/v1/acl/$service<[^/]+>/hepp") === "/api/v1/acl/{service}/hepp"
    self.getDocUri2("/api/v1/acl/$service<[^/]+>/hepp/") === "/api/v1/acl/{service}/hepp/"

    self.getDocUri2("/api/v1/acl/$service<[^/]+>/hepp") =/= "/api/v1/acl/{service}/hepp2"
    self.getDocUri2("/api/v1/acl/$service<[^/]+>/hepp") =/= "/api/v1/acl/{service2}/hepp"
    self.getDocUri2("/api/v1/acl/$service<[^/]+>/hepp") =/= "/api/v1/acl2/{service}/hepp"
    self.getDocUri2("/api/v1/acl/$service<[^/]+>/hepp") =/= "/2api/v1/acl/{service}/hepp"
    self.getDocUri2("/api/v1/acl/$service<[^/]+>/hepp2") =/= "/api/v1/acl/{service}/hepp"
    self.getDocUri2("/api/v1/acl/$service<[^/]+>/hepp/2") =/= "/api/v1/acl/{service}/hepp"
    self.getDocUri2("/api/v1/acl/$service2<[^/]+>/hepp/") =/= "/api/v1/acl/{service}/hepp"
    self.getDocUri2("/api/v1/acl2/$service<[^/]+>/hepp/") =/= "/api/v1/acl/{service}/hepp"
    self.getDocUri2("/api2/v1/acl/$service<[^/]+>/hepp/") =/= "/api/v1/acl/{service}/hepp"

    // two parameters in the middle of the uri:
    self.getDocUri2("/api/v1/acl/$service<[^/]+>/$hepp<[^/]+>") === "/api/v1/acl/{service}/{hepp}"
    self.getDocUri2("/api/v1/acl/$service<[^/]+>/gakk/$hepp<[^/]+>") === "/api/v1/acl/{service}/gakk/{hepp}"
  """)
  private def getDocUri2(confUri: String): String =
    if (confUri=="")
      ""
    else if (confUri.startsWith("$")) {
      val pos = confUri.indexOf("<")
      val pos2 = confUri.indexOf(">")
      val offset = pos2 - pos
      "{" + confUri.substring(1, pos) + "}" + getDocUri2(confUri.drop(pos+offset+1))
    } else if (confUri.startsWith("{")) {
      throw new Exception("""The URI in the play framework conf file can not contain a "{". Maybe you meant to use colon (":") instead?. Method: """"+restMethod+"""", Uri: """"+uri+'"')
    } else
      confUri.take(1) + getDocUri2(confUri.drop(1))

  /**
    * Returns the type of uri used in the docs.
    * @example {{{RouteEntry("", "/api/v1/acl/\$service<[^/]+>", "", "").getDocUri === "/api/v1/acl/{service}"}}}
    */
  def getDocUri: String =
    getDocUri2(uri)
}


object RoutesHelper{

  import play.api.Play.current

  // code in this method copied from the swagger play2 module.
  private def getRestClassName(annoDocField3: String) = {
    val m1 = annoDocField3.lastIndexOf("(") match {
      case i: Int if i > 0 => annoDocField3.substring(0, i)
      case _ => annoDocField3
    }
    m1.substring(0, m1.lastIndexOf(".")).replace("@", "")
  }

  @Test(code="""
    self.getRestMethodName("controllers.ApiDocController.get") === "get"
    self.getRestMethodName("controllers.ApiDocController.get(path:String)") === "get"
  """)
  private def getRestMethodName(annoDocField3: String) =
    if (annoDocField3.endsWith(")")) {
      val last = annoDocField3.lastIndexOf("(")
      val s2 = annoDocField3.take(last)
      val secondLast = s2.lastIndexOf(".")
      //println("annodoc: "+annoDocField3+", "+secondLast+", "+last+", "+s2.take(secondLast))
      s2.drop(secondLast+1)
    } else {
      val last = annoDocField3.lastIndexOf(".")
      annoDocField3.drop(last+1)
    }


  def getRouteEntries(): List[RouteEntry] =
    play.api.Play.routes.documentation.map(doc =>
      RouteEntry(doc._1, doc._2, getRestClassName(doc._3), getRestMethodName(doc._3))
    ).map(routeEntry =>
      //println("routeEntry: "+routeEntry)
      routeEntry
/*
    ).filter(routeEntry =>
      routeEntry.scalaClass != "controllers.Assets" &&
      routeEntry.scalaClass != "controllers.StaticFile" &&
      routeEntry.scalaClass != "controllers.Application" &&
      !routeEntry.uri.contains("<.+>")
 */
    ).toList

  @Test(code="""
    self.urisMatches("/api/v1/50", "/api/v1/50") === true
    self.urisMatches("/api/v1/50", "/api/v1/{haljapino}") === true
    self.urisMatches("/api/v1/50/fields", "/api/v1/{haljapino}/fields") === true

    self.urisMatches("/api/v1/50?hello=5",         "/api/v1/50") === true
    self.urisMatches("/api/v1/50?hello=5&gakk=6",  "/api/v1/{haljapino}") === true
    self.urisMatches("/api/v1/50/fields?hello=gakk", "/api/v1/{haljapino}/fields") === true

    self.urisMatches("/api/v1/", "/api/v1/{haljapino}") === false

    self.urisMatches("/api/v1/?hello=gakk", "/api/v1/{haljapino}") === false
  """)
  private def urisMatches(uri: String, docUri: String): Boolean = {
    val s_uri      = uri.split('?')(0).split("/")
    val s_routeUri = docUri.split("/")
    s_uri.size == s_routeUri.size &&
    s_uri.zip(s_routeUri).forall(g => {
      val uri_element = g._1
      val route_element = g._2
      uri_element == route_element || route_element.startsWith("{")
    })
  }

  /**
    *  @return a matching route given a list of routes, a method and a uri. Ignores appending slash and so forth.
    */
  @Test(code="""
    self.findMatchingRouteEntry("PUT", "/studier/api/v1/teaching-locations/0", List(no.samordnaopptak.apidoc.RouteEntry("PUT","/studier/api/v1/teaching-locations/$id<[^/]+>","class","method"))) === no.samordnaopptak.apidoc.RouteEntry("PUT","/studier/api/v1/teaching-locations/$id<[^/]+>","class","method")
  """)
  def findMatchingRouteEntry(
    method: String,
    uri: String,
    routes: List[RouteEntry]
  ): RouteEntry = {

    lazy val route = routes.head

    if(routes.isEmpty)
      throw new Exception("No matching route for "+method + " " + uri)
    else if (method==route.restMethod && urisMatches(uri, route.getDocUri))
      route
    else
      findMatchingRouteEntry(method, uri, routes.tail)
  }
}
