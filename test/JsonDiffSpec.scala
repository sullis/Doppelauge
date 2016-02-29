package test

import org.specs2.mutable.Specification

import play.api.libs.json._
import play.api.test.Helpers._

import no.samordnaopptak.json._
import no.samordnaopptak.json.JsonMatcher._
import no.samordnaopptak.json.JsonDiff._



class JsonDiffSpec extends Specification {

  "JsonDiff Object" should {

    "object 0" in {
      val json1 = J.obj()
      val json2 = J.obj()
      JsonMatcher.matchJson(
        JNoDiff,
        JsonDiff(json1, json2)
      )
    }

    "object 0" in {
      val json1 = J.obj("a" -> 1)
      val json2 = J.obj("a" -> 1)
      JsonMatcher.matchJson(
        JNoDiff,
        JsonDiff(json1, json2)
      )
    }

    "object 1" in {

      val json1 = J.obj(
        "a" -> 1,
        "b" -> 2
      )

      val json2 = J.obj(
        "a" -> 1,
        "b" -> 3
      )

      JsonMatcher.matchJson(
        J.obj(
          "b" -> JDiff(2, 3)
        ),
        JsonDiff(json1, json2)
      )
    }


    "object 2" in {

      val json1 = J.obj(
        "a" -> 1,
        "b" -> J.obj(
          "c" -> 2
        )
      )

      val json2 = J.obj(
        "a" -> 1,
        "b" -> J.obj(
          "c" -> 3
        )
      )

      JsonMatcher.matchJson(
        J.obj(
          "b" -> J.obj(
            "c" -> JDiff(2, 3)
          )
        ),
        JsonDiff(json1, json2)
      )
    }


    "object 3" in {

      val json1 = J.obj(
        "a" -> 1
      )

      val json2 = J.obj(
        "a" -> 1,
        "b" -> 2
      )

      JsonMatcher.matchJson(
        J.obj(
          "b" -> JObjectAdded(2)
        ),
        JsonDiff(json1, json2)
      )
    }


    "object 4" in {

      val json1 = J.obj(
        "a" -> 1,
        "b" -> 2
      )

      val json2 = J.obj(
        "a" -> 1
      )

      JsonMatcher.matchJson(
        J.obj(
          "b" -> JObjectRemoved(2)
        ),
        JsonDiff(json1, json2)
      )
    }


    "object 5" in {

      val json1 = J.obj(
        "a" -> 1,
        "b" -> 2
      )

      val json2 = J.obj(
        "b" -> 2
      )

      JsonMatcher.matchJson(
        J.obj(
          "a" -> JObjectRemoved(1)
        ),
        JsonDiff(json1, json2)
      )
    }


    "object 6" in {

      val json1 = J.obj(
        "a" -> 1,
        "b" -> 2,
        "c" -> 3
      )

      val json2 = J.obj(
        "a" -> 10,
        "c" -> 30
      )

      JsonMatcher.matchJson(
        J.obj(
          "a" -> JDiff(1, 10),
          "b" -> JObjectRemoved(2), // Test that order is kept.
          "c" -> JDiff(3, 30)
        ),
        JsonDiff(json1, json2)
      )
    }


    "object 7" in {

      val json1 = J.obj(
        "a" -> 1,
        "c" -> 3
      )

      val json2 = J.obj(
        "a" -> 10,
        "b" -> 20,
        "c" -> 30
      )

      JsonMatcher.matchJson(
        J.obj(
          "a" -> JDiff(1,10),
          "b" -> JObjectAdded(20), // Test that order is kept.
          "c" -> JDiff(3,30)
        ),
        JsonDiff(json1, json2)
      )
    }

    "object 8" in {
      val json1 = J.obj(
        "e" -> J.obj(
          "f" -> 50
        ),
        "c" -> J.obj(
          "d1" -> J.obj(),
          "d2" -> 90
        ),
        "a" -> J.obj()
      )

      val json2 = J.obj(
        "a" -> J.obj(),
        "c" -> J.obj(
          "d1" -> J.obj(),
          "d2" -> 90
        ),
        "e" -> J.obj(
          "f" -> 60
        )
      )

      JsonMatcher.matchJson(
        J.obj(
          "e" -> J.obj(
            "f" -> JDiff(50, 60)
          )
        ),
        JsonDiff(json1, json2)
      )
    }

  }


  "JsonDiff Array" should {

    "array 1" in {
      val json1 = J.arr()
      val json2 = J.arr()

      JsonMatcher.matchJson(
        JNoDiff,
        JsonDiff(json1, json2)
      )
    }

    "array 2" in {
      val json1 = J.arr(1)
      val json2 = J.arr(1)

      JsonMatcher.matchJson(
        JNoDiff,
        JsonDiff(json1, json2)
      )
    }

    "array 3" in {
      val json1 = J.arr(1)
      val json2 = J.arr()

      JsonMatcher.matchJson(
        J.arr(
          JArrayRemoved(0, 1)
        ),
        JsonDiff(json1, json2)
      )
    }

    "array 4" in {
      val json1 = J.arr()
      val json2 = J.arr(1)

      JsonMatcher.matchJson(
        J.arr(
          JArrayAdded(0, 1)
        ),
        JsonDiff(json1, json2)
      )
    }

    "array 5" in {
      val json1 = J.arr(1,2,3)
      val json2 = J.arr(1,3)

      JsonMatcher.matchJson(
        J.arr(
          JArrayRemoved(1,2)
        ),
        JsonDiff(json1, json2)
      )
    }

    "array 6" in {
      val json1 = J.arr(1,3)
      val json2 = J.arr(1,2,3)

      //println("diff 6: "+JsonDiff(json1, json2))
      JsonMatcher.matchJson(
        J.arr(
          JArrayAdded(1,2)
        ),
        JsonDiff(json1, json2)
      )
    }

    "array 7" in {
      val json1 = J.arr(1,2,3)
      val json2 = J.arr(1,20,3)

      //println("diff 7: "+JsonDiff(json1, json2))
      JsonMatcher.matchJson(
        J.arr(
          JArrayChanged(1,JDiff(2,20))
        ),
        JsonDiff(json1, json2)
      )
    }

    "array 8" in {
      val json1 = J.arr(1,2,3,4,5)
      val json2 = J.arr(1,  3,4,5)

      JsonMatcher.matchJson(
        J.arr(
          JArrayRemoved(1,2)
        ),
        JsonDiff(json1, json2)
      )
    }

    "array 9" in {
      val json1 = J.arr(1,  3,4,5)
      val json2 = J.arr(1,2,3,4,5)

      JsonMatcher.matchJson(
        J.arr(
          JArrayAdded(1,2)
        ),
        JsonDiff(json1, json2)
      )
    }

    "array 10" in {
      val json1 = J.arr(1,2,3,4,5)
      val json2 = J.arr(1,20,3,4,5)

      JsonMatcher.matchJson(
        J.arr(
          JArrayChanged(1,JDiff(2,20))
        ),
        JsonDiff(json1, json2)
      )
    }

    "array 11" in {
      val json1 = J.arr("a","b","c","d","e")
      val json2 = J.arr("e","b","d","a","c")

      JsonMatcher.matchJson(
        J.arr(
          JArrayMoved(4,0,"e"),
          JArrayMoved(3,2,"d"),
          JArrayMoved(0,3,"a"),
          JArrayMoved(2,4,"c")
        ),
        JsonDiff(json1, json2)
      )
    }

    //feil. "e1" har fått pos 4, og ikke 5.
    "array 12" in {
      val json1 = J.arr(     "a", "b", "c", "d", "e1")
      val json2 = J.arr(50,  "a", "b", "c", "d", "e2")

      JsonMatcher.matchJson(
        J.arr(
          JArrayAdded(0, 50),
          JArrayChanged(5, JDiff("e1", "e2"))
        ),
        JsonDiff(json1, json2)
      )
    }

    "array 13" in {
      val json1 = J.arr(50, "a", "b", "c", "d", "e")
      val json2 = J.arr(    "a", "b", "c", "d", "e")

      JsonMatcher.matchJson(
        J.arr(
          JArrayRemoved(0, 50)
        ),
        JsonDiff(json1, json2)
      )
    }

    "array 14" in {
      val json1 = J.arr(50, 60, "a", "b", "c", "d", "e1")
      val json2 = J.arr(        "a", "b", "c", "d", "e2")

      //println("diff 14: "+JsonDiff(json1,json2))
      //true

      JsonMatcher.matchJson(
        J.arr(
          JArrayRemoved(0, 50),
          JArrayRemoved(1, 60),
          JArrayChanged(4, JDiff("e1", "e2"))
        ),
        JsonDiff(json1, json2)
      )
    }

    "array 15" in {
      val json1 = J.arr("50", "b", "c1", "e1", "f") //, "g1", "h1", "i")
      val json2 = J.arr(      "b", "c2", "e2", "f") //, "g2", "h2", "i")
/*
       0     1     2     3     4
      --------------------------
      50,    b ,  c1,   e1,   f
      b,    c2 ,  e2,   f
      ->
      50,   b,    c1,   e1,   f
      b,   c2,    e2,   f

      50,    b ,  c1,   e1,   f
      b,    c2 ,  e2,   f
      ->
      50,  c1,   e1
      c2,  e2

      // Remove all MOVE.before elements:
      50,    b ,  c1,   e1,   f
      b,    c2 ,  e2,   f
      ->
      50,   c1,   e1
      b,    c2 ,  e2,   f

      // Remove all MOVE.before elements 2:
      50,    b ,  c1,   e1,   f,   g1,   h,   i
      b,    c2 ,  e2,   f,   g2,   60,   70,   h,    i
      ->
50,   b ,   c1,   e1,   f,   g1,   h,    i                      <  At pos 1: skip -1 step (b.pos_after-b.pos_before = 0-1) (skip the skip if there are MOVE elements in between pos_before and pos_after)
      b,    c2 ,  e2,   f,   g2,   60,   70,   h,    i
      ->
50,   b ,   c1,   e1,   f,   g1,               h,    i          <- At pos 6: skip +1 step (h.pos_after-h.pos_before = 7-6)
      b,    c2 ,  e2,   f,   g2,   60,   70,   h,    i


 */
      //println("diff 15: "+JsonDiff(json1,json2))
      //true

      JsonMatcher.matchJson(
        J.arr(
          JArrayRemoved(0, "50"),
          JArrayChanged(1, JDiff("c1", "c2")),
          JArrayChanged(2, JDiff("e1", "e2"))
        ),
        JsonDiff(json1, json2)
      )

    }

    "array 16" in {
      val json1 = J.arr(50,    "b" ,  "c1",   "e1",   "f",   "g1",               "h",   "i")
      val json2 = J.arr(       "b",   "c2",   "e2",   "f",   "g2",   60,   70,   "h",    "i")

      //println("diff 16: "+JsonDiff(json1,json2))
      //true

      JsonMatcher.matchJson(
        J.arr(
          JArrayRemoved(0, 50),
          JArrayChanged(1, JDiff("c1", "c2")),
          JArrayChanged(2, JDiff("e1", "e2")),
          JArrayChanged(4, JDiff("g1", "g2")),
          JArrayAdded(5, 60),
          JArrayAdded(6, 70)
        ),
        JsonDiff(json1, json2)
      )

    }
  }

  "Array + Object" should {
    "arrayobject 8" in {
      val json1 = J.obj(
        "a" -> J.arr(
          50
        )
      )

      val json2 = J.obj(
        "a" -> J.arr(
          60
        )
      )

      //println("arrayobject 8: "+JsonDiff(json1, json2))

      JsonMatcher.matchJson(
        J.obj(
          "a" -> J.arr(
            JArrayChanged(0, JDiff(50,60))
          )
        ),
        JsonDiff(json1, json2)
      )
    }


    "createHTML" should {
      val json1 = J.obj(
        "a" -> 1,
        "b" -> 100,
        "f" -> List(2222,3000,4000,"al")
      )

      val json2 = J.obj(
        "a" -> 10,
        "c" -> 90,
          "f" -> List(2222,5000,3000,9000)
      )

      val html_code = JsonDiff.createHtml(json1, json2)

      def firefox(data: String) = {
        val w = new java.io.BufferedWriter(new java.io.OutputStreamWriter(new java.io.FileOutputStream("/tmp/tmp.html"), "UTF-8"))
        w.write(data)
        w.close()
          java.lang.Runtime.getRuntime().exec("firefox /tmp/tmp.html")
        //println("Just stored "+data.size+" letters to " + getFilename(key))
      }


      // uncomment line below to see result in firefox
      //firefox(html_code)

      html_code.replace(" ","") === ("""
        <div style="background-color:#eeeedd">
      <table border=1>
      <tr>
        <th>Path</th>
        <th>Old value</th>
        <th>New value</th>
      </tr>
      <tr><td><span style="background-color:#bbbbbb">a</span></td><td>1</td><td>10</td></tr><tr><td><span style="background-color:#ff8888">b</span></td><td>100</td><td></td></tr><tr><td><span style="background-color:#88ff88">f[1]</span></td><td></td><td>5000</td></tr><tr><td><span style="background-color:#ff8888">f[2]</span></td><td>4000</td><td></td></tr><tr><td><span style="background-color:#ff8888">f[3]</span></td><td>"al"</td><td></td></tr><tr><td><span style="background-color:#88ff88">f[3]</span></td><td></td><td>9000</td></tr><tr><td><span style="background-color:#88ff88">c</span></td><td></td><td>90</td></tr>
      </table>
        </div>
      """).replace(" ","")

    }

  }
}
