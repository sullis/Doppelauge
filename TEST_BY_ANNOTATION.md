
TestByAnnotation
================



Problem
-------

Unit tests often becomes too detailed, and they often breaks too easily.


Solution
--------

By putting tests directly above the method themselves, it becomes
natural to update the tests simultaneously as the code. The tests
also starts to serve as documentation for how the method works.


Example
-------

```scala
// Production code:

import no.samordnaopptak.TestByAnnotation.Test

object StringModifier{
  @Test(code="""
     self.appendUnderscore("a")  =/= "a"
     self.appendUnderscore("a")  === "a_"
     self.appendUnderscore("a2") === "a2_"
  """)
  private def appendUnderscore(input: String): String =
    input+"_"
}


// Test code:

import org.specs2.mutable._
import no.samordnaopptak.TestByAnnotation

class TestByAnnotationSpec extends Specification {
  "TestByAnnotation" should {
    "Test by annotation the 'StringModifier' object" in {
       TestByAnnotation.TestObject(StringModifier)
    }
  }
}
```


Scala-doc
---------
http://folk.uio.no/ksvalast/doppelauge/#no.samordnaopptak.test.TestByAnnotation.TestObject$
