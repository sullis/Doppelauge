package test

import no.samordnaopptak.apidoc.TestByAnnotation._

import org.specs2.mutable._


object ObjectToTestWith {
  
  @Test(code="""
     self.test("a") =/= "a22"
     self.test("a")  === "a_"
     self.test("a2") === "a2_"
  """)
  def test(input: String): String =
    input+"_"


  @Test(code="""
    self.test2("a",List(5,2,3)) === List("b")
    self.test2("a",List(5,2,3)) === List("b")
    self.test2("a",List(5,2,3)) =/= List()
  """)
  def test2(input: String, b: List[Int]): List[String] =
    List("b")


  def notest() = None
}


class TestByAnnotationSpec extends Specification {
  "TestByAnnotation" should {
    "not fail the annotation tests" in {
      TestObject(ObjectToTestWith)
      true === true
    }
  }
}
