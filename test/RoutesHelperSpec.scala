package test

import org.specs2.mutable._

import no.samordnaopptak.test.TestByAnnotation._

import no.samordnaopptak.apidoc.{RouteEntry, RoutesHelper}


class RoutesHelperSpec extends Specification with InjectHelper {

  lazy val routesHelper = inject[RoutesHelper]

  "RoutesHelper" should {

    "pass the annotation tests in RoutesHelper" in {
      TestObject(routesHelper)
    }

    "pass the annotation tests in RouteEntry" in {
      TestObject(RouteEntry("", "", "", ""))
    }
  }

}
