package test

import no.samordnaopptak.apidoc.TestByAnnotation._

import org.specs2.mutable._


class ClassToTestWith {
  
  @Test(code="""
     self.test("a")  =/= "a2_"
     self.test("a")  === "a_"
     self.test("a2") === "a2_"
     instance.test("a2") === "a2_" // "instance" contains the object itself. I.e. there is no reflection magic going on in order to call private methods.
  """)
  def test(input: String): String =
    input+"_"


  @Test(code="""
    self.test2("a",List(5,2,3)) =/= List()
    self.test2("a",List(5,2,3)) === List("b")
    self.test2("a",List(5,2,3)) === List("b")
  """)
  private def test2(input: String, b: List[Int]): List[String] =
    List("b")

  // Check that dollars can be used
  @Test(code="""
     self.test3("$a") =/= "$ab"
  """)
  private def test3(input: String): String = {
    assert(input=="$a")
    input
  }

  def notest() = None
}


class TestByAnnotationSpec extends Specification {
  "TestByAnnotation" should {

    "test the annotation test in TestObject itself" in {
      TestObject(TestObject)
      true === true
    }

    "not fail the annotation tests" in {
      TestObject(new ClassToTestWith)
      true === true
    }

  }

}
