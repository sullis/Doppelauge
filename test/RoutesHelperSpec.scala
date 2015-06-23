package test

import org.specs2.mutable._

import no.samordnaopptak.apidoc.TestByAnnotation._
import no.samordnaopptak.apidoc.RoutesHelper


class RoutesHelperSpec extends Specification {
  "RoutesHelper" should {

    "pass the annotation tests" in {
      TestObject(RoutesHelper)
      true === true
    }
  }

}
