package test

import org.specs2.mutable._

import no.samordnaopptak.test.TestByAnnotation._

import no.samordnaopptak.apidoc.{RouteEntry, RoutesHelper}


class RoutesHelperSpec extends Specification {
  "RoutesHelper" should {

    "pass the annotation tests in RoutesHelper" in {
      TestObject(RoutesHelper)
    }

    "pass the annotation tests in RouteEntry" in {
      TestObject(RouteEntry("", "", "", ""))
    }
  }

}
