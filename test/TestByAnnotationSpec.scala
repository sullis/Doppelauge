package test

import org.specs2.mutable.Specification

import no.samordnaopptak.test.TestByAnnotation.{Test, TestObject}


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

  @Test(code="""
    self.testAdder() === true
  """)
  private def testAdder() = {
    implicit class Hepp[T](val a: T) {
      def ===(b: T) = if (a!=b) throw new Exception(s"$a != $b") else true
    }

    Adder(5).one === 6
    Adder(5).two === 7
  }

  private case class Adder(value: Int){
    def one = value+1
    def two = value+2
  }

}


class TestByAnnotationSpec extends Specification {
  "TestByAnnotation" should {

    "test the annotation test in TestObject itself" in {
      TestObject(TestObject)
    }

    "not fail the annotation tests" in {
      TestObject(new ClassToTestWith)
    }

  }

}
