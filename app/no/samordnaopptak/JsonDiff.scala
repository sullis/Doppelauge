package no.samordnaopptak.json

import scala.annotation.tailrec


/**
  * Very simple Json Diff algorithm.
  * 
  * Comparing json objects is straight forward. Comparing json arrays is harder.
  * 
  * Traditional diff algorithms such as Myers (https://neil.fraser.name/software/diff_match_patch/myers.pdf)
  * is likely not to produce what you want when comparing json arrays. Usually:
  * 1. Order doesn't matter,
  * 2. Two elements can be extremely similar (e.g. only something small deep down has changed),
  *    and we rather want to compare those elements deep down, not just say that this one has been added and that one has been removed.
  * 
  * The challenge when comparing json arrays is to find out which of the 'Add' and 'Remove' elements that are actually changed elements ('Change').
  * This algorithm uses the positions of 'Move' elements to make guesstimations. The algorithm is rather (e.g. "too") simple, but
  * at least it works perfectly for the tests in JsonDiffSpec.scala (!), although probably not quite so perfectly
  * well for all real life situations. Ideally, we would probably compare all elements against
  * all others, and transform those 'Add' and 'Remove' elements into 'Change' elements which are most similar, but that work has not been done yet.
  * 
  * The quite disastreous situation not covered here, is if an element is both moved and modified, then the output could
  * be very verbose.
  * 
  */
object JsonDiff {


  case class JDiff(a: Any, b: Any) extends JValue{
    def asJsValue = J.arr(
      "____________diff_changed",
      J.obj(
        "before" -> a,
        "after" -> b
      )
    ).asJsValue
  }

  case class JObjectRemoved(json: Any) extends JValue{
    def asJsValue = J.arr(
      "____________diff_removed",
      J.obj(
        "value" -> json
      )
    ).asJsValue
  }

  case class JObjectAdded(json: Any) extends JValue{
    def asJsValue = J.arr(
      "____________diff_added",
      J.obj(
        "value" -> json
      )
    ).asJsValue
  }

 case class JArrayChanged(pos: Int, diff: JValue) extends JValue{
    def asJsValue = J.arr(
      "____________diff_array_element_changed",
      J.obj(
        "pos" -> pos,
        "diff" -> diff
      )
    ).asJsValue
  }

  case class JArrayAdded(pos: Int, json: Any) extends JValue{
    def asJsValue = J.arr(
      "____________diff_array_element_added",
      J.obj(
        "pos" -> pos,
        "value" -> json
      )
    ).asJsValue
  }

  case class JArrayRemoved(pos: Int, json: Any) extends JValue{
    def asJsValue = J.arr(
      "____________diff_array_element_removed",
      J.obj(
        "pos" -> pos,
        "value" -> json
      )
    ).asJsValue
  }

  case class JArrayMoved(pos_before: Int, pos_after: Int, json: Any) extends JValue{
    def asJsValue = J.arr(
      "____________diff_array_element_moved",
      J.obj(
        "pos_before" -> pos_before,
        "pos_after" -> pos_after,
        "value" -> json
      )
    ).asJsValue
  }

  object JNoDiff extends JArray(List(JString("____________diff_no_diff")))




  private def diffObject(json_a: JObject, json_b: JObject): JValue = {
    val keys_a = json_a.keys
    val keys_b = json_b.keys

    def remove_element(key: String, list: List[(String, JValue)]): List[(String, JValue)] =
      if (list.head._1==key)
        list.tail
      else
        list.head :: remove_element(key, list.tail)

    @tailrec def loop(aaList: List[(String, JValue)], bb: List[(String, JValue)], result: List[(String, JValue)]): List[(String, JValue)] = (aaList, bb) match {
      case (`Nil`, _) =>
        result.reverse ++
        bb.map {
          case (key_a: String, value_a: JValue) => key_a -> JObjectAdded(value_a)
        }

      case (aa   , `Nil`) =>
        result.reverse ++
        aa.map {
          case (key_a: String, value_a: JValue) => key_a -> JObjectRemoved(value_a)
        }

      case ((key_a, value_a) :: rest_a, (key_b, value_b) :: rest_b) =>

        // Scala couldn't make 'loop' tail recursive when it was called through this helper function.
        /*
        def runDiff(new_value_b: JValue, new_rest_b: List[(String, JValue)]) =
          apply(value_a, new_value_b) match {
            case `JNoDiff`     => loop(rest_a, new_rest_b, result)
            case diff: JValue  => loop(rest_a, new_rest_b, (key_a -> diff) :: result)
          }
         */

        if (key_a == key_b) {

          val diff = apply(value_a, value_b)
          if (diff == JNoDiff)
            loop(rest_a, rest_b, result)
          else
            loop(rest_a, rest_b, (key_a -> diff) :: result)

        } else if(keys_b.contains(key_a)) {

          val diff = apply(value_a, json_b(key_a) )
          val new_rest_b = remove_element(key_a, bb)

          if (diff == JNoDiff)
            loop(rest_a, new_rest_b, result)
          else
            loop(rest_a, new_rest_b, (key_a -> diff) :: result)

        } else
          loop(rest_a, bb, (key_a -> JObjectRemoved(value_a)) :: result)

    }

    val loop_result = loop(json_a.asMap.toList, json_b.asMap.toList, Nil)

    if (loop_result.isEmpty)
      JNoDiff
    else
      JObject(loop_result) // Use JObect instead of J(loop(...).toMap) to ensure field order.
  }


  private def diffArray(json_a: JArray, json_b: JArray): JValue = {

    trait Diff {
      def toJDiff: JValue
      val sort_pos: Int
    }

    case class Add(value: JValue, pos: Int) extends Diff {
      val sort_pos = pos
      def toJDiff = JArrayAdded(pos, value)
    }

    case class Remove(value: JValue, pos: Int, org_pos: Int) extends Diff {
      val sort_pos = org_pos
      def toJDiff = JArrayRemoved(org_pos, value)
    }

    case class Move(value: JValue, pos_before: Int, pos_after: Int, org_pos_before: Int, maybe_be_removed: Boolean = true) extends Diff {
      def toJDiff = JArrayMoved(org_pos_before, pos_after, value)
      val sort_pos = pos_after // Sort by pos_before or pos_after? Since everyone else are sorted by final position, it should probably pos_after here, not pos_before. But it's a bit confusing.

      val forward = pos_before < pos_after

      def covers(another_move: Move) =
        if (forward)
          pos_before <= another_move.pos_before   &&   pos_after  >= another_move.pos_after
        else
          pos_after  <= another_move.pos_after    &&   pos_before >= another_move.pos_before
    }

    case class Change(value_before: JValue, value_after: JValue, pos: Int) extends Diff {
      val sort_pos = pos
      def toJDiff = JArrayChanged(pos, apply(value_before, value_after))
    }

    // Note that 'moves_are_skewed' will return the wrong value (i.e. true instead of false) unless move1 and move2 have at least 1 position in common.
    // We don't check for that situation since it's part of a larger optimization strategy not to call this function for unrelated moves.
    def moves_are_skewed(move1: Move, move2: Move) =
      if (move1 == move2)
        true
      else if (move1.forward && move2.forward)
        !(move1.covers(move2) || move2.covers(move1)) // i.e. if they both move forward, they are skewed compared to each other, unless one of them covers the other completely.
      else if (!move1.forward && !move2.forward)
        !(move1.covers(move2) || move2.covers(move1)) // same thing when moving backwards as forwards.
      else
        false // I.e if they move in opposite directions, they are definitely not skewed compared to each other

    def mark_unremoving_moves(diffs: List[Diff]) = {

      def find_iter_range(move: Move) =
        if (move.forward)
          move.pos_before to move.pos_after
        else
          move.pos_after to move.pos_before

      @tailrec def find_moves(diffs: List[Diff], result: List[Move] = List()): List[Move] = diffs match {
        case `Nil`                => result.reverse
        case ( (d: Move) :: rest) => find_moves(rest, d::result)
        case _           :: rest  => find_moves(rest, result)
      }

      def find_ranges(diffs: List[Diff]): Map[Int, List[Move]] =
        find_moves(diffs)                                        // Move(2,4), Move(1,5)
          .flatMap(move => find_iter_range(move).map((_, move))) // List((2,Move(2,4)), (3,Move(2,4)), (4,Move(2,4)), (1,Move(1,5)), ..., (5,Move(1,5))
          .groupBy(_._1)                                         // Map( 2 -> ..., 3, ..., ...)
          .map{
            case (key, list) => key -> list.map(_._2).toList
          }

      val ranges = find_ranges(diffs)
      //println("ranges: "+ranges)

      def is_not_skewed_with_another_overlapping_move(move: Move) = {
        val iter_range = find_iter_range(move)

        val overlapping_moves = iter_range.flatMap(pos => ranges(pos)).toSet

        overlapping_moves.exists(overlapping_move => !moves_are_skewed(overlapping_move, move))
      }

      @tailrec def mark(diffs: List[Diff], result: List[Diff] = List()): List[Diff] = diffs match {
        case `Nil`                                                                  => result.reverse
        case ( (m: Move) :: rest) if is_not_skewed_with_another_overlapping_move(m) => mark(rest, m.copy(maybe_be_removed=false) :: result)
        case ( d         :: rest)                                                   => mark(rest, d                              :: result)
      }

      mark(diffs)
    }

    @tailrec def remove_unnecessary_moves(diffs: List[Diff], result: List[Diff] = List()): List[Diff] = diffs match {
      case `Nil`                                            => result.reverse
      case (m: Move) :: rest if m.pos_before == m.pos_after => remove_unnecessary_moves(rest, result)
      case d         :: rest                                => remove_unnecessary_moves(rest, d::result)
    }

    @tailrec def convert_as_much_as_possible_to_Changes(diffs: List[Diff], result: List[Diff] = List()): List[Diff] = diffs match {
      case `Nil`                                           => result.reverse
      case a                       :: `Nil`                => (a::result).reverse
      case (a: Remove) :: (b: Add) :: aa if a.pos == b.pos => convert_as_much_as_possible_to_Changes(aa, Change(a.value, b.value, a.pos) :: result)
      case (a: Add) :: (b: Remove) :: aa if a.pos == b.pos => convert_as_much_as_possible_to_Changes(aa, Change(b.value, a.value, b.pos) :: result)
      case a                       :: aa                   => convert_as_much_as_possible_to_Changes(aa, a::result)
    }

    def inbetween(a: Int, b: Int, c: Int) =
      a <= b && b <= c

    def no_move_inbetween(diffs: List[Diff], pos1: Int, pos2: Int): Boolean = diffs match {
      case `Nil`                                                    => true
      case (m: Move) :: rest if inbetween(pos1, m.pos_before, pos2) => false
      case (m: Move) :: rest if inbetween(pos1, m.pos_after,  pos2) => false
      case d         :: rest                                        => no_move_inbetween(rest, pos1, pos2)
    }

    @tailrec def adjust_before_pos(diffs: List[Diff], result: List[Diff] = List(), skew: Int = 0, dontconvertpos: Int = -1): List[Diff] = diffs match {
      case `Nil`                                     => result.reverse
      case (m: Move)   :: rest if m.maybe_be_removed => adjust_before_pos(rest, m.copy(pos_before = m.pos_after )        :: result, m.pos_after - m.pos_before)
      case (m: Move)   :: rest                       => adjust_before_pos(rest, m.copy(pos_before = m.pos_before + skew) :: result, skew)
      case (r: Remove) :: rest                       => adjust_before_pos(rest, r.copy(pos        = r.pos + skew)        :: result, skew)
      case d           :: rest                       => adjust_before_pos(rest, d                                        :: result, skew)
    }

    @tailrec def perhaps_find_and_remove_an_Add(value: JValue, diffs: List[Diff], result: List[Diff]): Option[(Add,List[Diff])] =
      diffs match {
        case `Nil`                               => None
        case (a: Add) :: aa   if a.value==value  => Some(a, result.reverse ++ aa)
        case a        :: aa                      => perhaps_find_and_remove_an_Add(value, aa, a::result)
      }

    @tailrec def perhaps_find_and_remove_a_Remove(value: JValue, diffs: List[Diff], result: List[Diff]): Option[(Remove,List[Diff])] =
      diffs match {
        case `Nil`                                  => None
        case (a: Remove) :: aa   if a.value==value  => Some(a, result.reverse ++ aa)
        case a           :: aa                      => perhaps_find_and_remove_a_Remove(value, aa, a::result)
      }

    @tailrec def convert_as_much_as_possible_to_Moves(diffs: List[Diff], result: List[Diff] = List()): List[Diff] = diffs match {
      case `Nil` => result.reverse

      case (remove: Remove) :: aa =>
        perhaps_find_and_remove_an_Add(remove.value, aa, Nil) match {
          case None                 =>  convert_as_much_as_possible_to_Moves(aa,     remove                                                  :: result)
          case Some((add, new_aa))  =>  convert_as_much_as_possible_to_Moves(new_aa, Move(remove.value, remove.pos, add.pos, remove.org_pos) :: result)
        }

      case (add: Add) :: aa =>
        perhaps_find_and_remove_a_Remove(add.value, aa, Nil) match {
          case None                    =>  convert_as_much_as_possible_to_Moves(aa,     add                                                     :: result)
          case Some((remove, new_aa))  =>  convert_as_much_as_possible_to_Moves(new_aa, Move(remove.value, remove.pos, add.pos, remove.org_pos) :: result)
        }

      case _ => throw new Exception("internal error. something is wrong")
    }

    @tailrec def find_Adds_and_Removes(pos: Int, aaJList: List[JValue], bbJList: List[JValue], result: List[Diff] = List()): List[Diff] = (aaJList, bbJList) match {
      case   (  `Nil`,   `Nil`  )          =>   result.reverse
      case   (  `Nil`,   b::bb  )          =>   find_Adds_and_Removes(pos+1, Nil, bb,                  Add(b, pos)         :: result)
      case   (  a::aa,   `Nil`  )          =>   find_Adds_and_Removes(pos+1, aa,  Nil,                 Remove(a, pos, pos) :: result)
      //case   (  a::aa,   b::bb  ) if a==b  =>   find_Adds_and_Removes(pos+1, aa,  bb,                                    result) // This looks wrong...
      case   (  a::aa,   b::bb  )          =>   find_Adds_and_Removes(pos+1, aa,  bb,   Add(b, pos) :: Remove(a, pos, pos) :: result)
    }


    var diffs = find_Adds_and_Removes(0, json_a.asArray.toList, json_b.asArray.toList)
    diffs = convert_as_much_as_possible_to_Moves(diffs)
    diffs = mark_unremoving_moves(diffs)
    diffs = adjust_before_pos(diffs)
    diffs = remove_unnecessary_moves(diffs)
    diffs = convert_as_much_as_possible_to_Changes(diffs)
    diffs = diffs.sortBy(_.sort_pos)

    //println("SORTED: "+diffs)

    if (diffs.isEmpty)
      JNoDiff
    else
      JArray(diffs.map(_.toJDiff))
  }

  /**
    * Creates a json diff value from two json value. Look at the source code for [[createHtml]] for an example on how to parse the returned value.
    */
  def apply(aJValue: JValue, bJValue: JValue): JValue = {

    (aJValue, bJValue) match {

      case (a: JObject,  b: JObject )         => diffObject(a, b)
      case (a: JArray,   b: JArray  )         => diffArray(a, b)

      case (a,           b          ) if a!=b => JDiff(a, b)

      case (_,           _          )         => JNoDiff
    }
  }


  /**
    *  Creates an HTML table showing the difference between two json values.
    *  @return An html string
    */
  def createHtml(
    a: JValue,
    b: JValue,
    jsonValuePrinter : JValue => String = _.pp(),
    green_color: String = "#88ff88",
    red_color: String = "#ff8888",
    grey_color: String = "#bbbbbb",
    main_background_color: String = "#eeeedd",
    table_headers: (String, String, String) = ("Path", "Old value", "New value")
  ): String = {

    object HtmlDiff {

      def background_color(color: String, text: String) =
        s"""<span style="background-color:$color">""" + text + "</span>"

      def green(text: String) =
        background_color(green_color, text)

      def red(text: String) =
        background_color(red_color, text)

      def grey(text: String) =
        background_color(grey_color, text)

      def tag(tag_name: String, text: String) =
        "<" + tag_name + ">" + text + "</" + tag_name + ">"

      def quotify(text: String) =
        "\"" + text + "\""

      def prettyPrintPath(path: String) =
        if (path.startsWith("."))
          path.drop(1)
        else
          path

      def get_table_row(path: String, old_value: String, new_value: String): String = {
        val prettyPrintedPath = prettyPrintPath(path)
        tag("tr", "" ++
          tag("td", if (old_value!="" && new_value!="") grey(prettyPrintedPath) else if (old_value!="") red(prettyPrintedPath) else green(prettyPrintedPath)) ++
          tag("td", old_value) ++
          tag("td", new_value)
        )
      }

      trait Diff{
        def toHtml: String
      }

      case class Removed(path: String, value: JValue) extends Diff {
        def toHtml = get_table_row(path, jsonValuePrinter(value), "")
      }

      case class Added(path: String, value: JValue) extends Diff {
        def toHtml = get_table_row(path, "", jsonValuePrinter(value))
      }

      case class Changed(path: String, old_value: JValue, new_value: JValue) extends Diff {
        def toHtml = get_table_row(path, jsonValuePrinter(old_value), jsonValuePrinter(new_value))
      }

      def getDiffs(diff: JObject, path: String): List[Diff] =
        diff.asMap.toList.flatMap {
          case (key, diff: JObjectAdded)   => List(Added(path + "." + key, J(diff.json)))
          case (key, diff: JObjectRemoved) => List(Removed(path + "." + key, J(diff.json)))
          case (key, value: JValue)        => getDiffs(value, path + "." + key)
        }

      def getDiffs(diff: JArray, path: String): List[Diff] =
        diff.asArray.toList.flatMap {
          case (diff: JArrayAdded)   => List(Added(path + "[" + diff.pos + "]", J(diff.json)))
          case (diff: JArrayRemoved) => List(Removed(path + "[" + diff.pos + "]", J(diff.json)))
          case (diff: JArrayMoved)   => List() // Probably not important to report.
          case (diff: JArrayChanged) => getDiffs(diff.diff, path + "[" + diff.pos + "]")
        }

      def getDiffs(diff: JValue, path: String = ""): List[Diff] =
        diff match {

          case (diff: JObject)     => getDiffs(diff, path)
          case (diff: JArray)      => getDiffs(diff, path)

          case (diff: JDiff)       => List(Changed(path, J(diff.a), J(diff.b)))

          case (diff: JValue)      => List()
        }

      def getTableRows(diff: JValue) =
        getDiffs(diff).map(_.toHtml).mkString
    }

    val jsonDiff = JsonDiff(a, b)
    val html_code = HtmlDiff.getTableRows(jsonDiff)

    if (html_code == "")
      ""
    else
      s"""
    <div style="background-color:$main_background_color">
      <table border=1>
      <tr>
        <th>${table_headers._1}</th>
        <th>${table_headers._2}</th>
        <th>${table_headers._3}</th>
      </tr>
      $html_code
      </table>
    </div>
    """
  }
}

