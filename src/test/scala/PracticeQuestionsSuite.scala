package questions

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import questions.PracticeQuestions._

@RunWith(classOf[JUnitRunner])
class PracticeQuestionsSuite extends FunSuite {


  def nullOrEmptyListTests(f: (List[Any]) => Any) {
    assertThrows[IllegalArgumentException] {
      f(null)
    }
    assertThrows[IllegalArgumentException] {
      f(List())
    }
  }

  test("avg: ") {

    val list = List(1.0, 2.0, 3.0)
    assert(avg(list) == 2.0, "Should be 2.0")

  }



  test("P01: last(as: List[Any])") {

    nullOrEmptyListTests(last)

    val list = List(1, 1, 2, 3, 5, 8)

    assert(last(list) == 8, "Should be 8")
    assert(last(List(1)) == 1, "Should be 1")

  }

  test("avg") {

    val list = List(1.0, 2.0, 3.0)

    assert(avg(list) == 2.0, "Should be 2.0")

  }


  test("P02: penultimate(as: List[Any])") {

    nullOrEmptyListTests(penultimate)

    val list = List(1, 1, 2, 3, 5, 8)

    assert(penultimate(list) == 5, "Should be 5")
    assert(penultimate(List(1, 2)) == 1, "Should be 1")
    assert(penultimate(List("Hello", 'b, 2)) == 'b, "Should be 'b")
    assert(penultimate(List(12.3, 'c, "Hello", 2)) == "Hello", """Should be "Hello" """)

    assertThrows[IllegalArgumentException] {
      penultimate(List(1))
    }
  }

  test("P03: kth(k: Int, as: List[Any])") {

    def kthSet(as: List[Any]) = kth(0, as)

    nullOrEmptyListTests(kthSet)

    val list = List(1, 1, 2, 3, 5, 8)

    assert(kth(0, list) == 1, "Should be 1")
    assert(kth(1, list) == 1, "Should be 1")
    assert(kth(3, list) == 3, "Should be 3")
    assert(kth(5, list) == 8, "Should be 8")

    assertThrows[IllegalArgumentException] {
      assert(kth(6, list) == 0)
    }

  }

  test("P04: length(as: List[Any])") {

    assertThrows[IllegalArgumentException] {
      assert(length(null) == 0)
    }

    val list = List(1, 1, 2, 3, 5, 8)

    assert(length(list) == 6, "Should be 6")
    assert(length(List()) == 0, "Should be 0")

  }

  test("P05: reverse(l: List[Any]): List[Any]") {

    //    assertThrows[IllegalArgumentException] {
    //      assert(reverse(null) == List())
    //    }

    val list = List(1, 1, 2, 3, 5, 8)
    assert(reverse(list) == List(8, 5, 3, 2, 1, 1), "Should be reversed")
    assert(reverse(List(2)) == List(2), "Should be the same")
    assert(reverse(List(1, "1")) == List("1", 1), "Should be reversed")
    assert(reverse(List()) == List(), "Should be empty list")

  }

  test("P06: isPalindrome(l: List[Any]): Boolean") {

    def doTests(f: List[Any] => Boolean) {

      assertThrows[IllegalArgumentException] {
        assert(!f(null))
      }

      assert(f(List()), "Should be a palindrome.")
      assert(f(List(1)), "Should be a palindrome.")
      assert(f(List(1, 1)), "Should be a palindrome.")
      assert(f(List(1, 2, 1)), "Should be a palindrome.")
      assert(f(List(1, 2, 2, 1)), "Should be a palindrome.")
      assert(!f(List(1, 2)), "Should not be a palindrome.")

    }

    doTests(isPalindrome)
    doTests(isPalindromeB)
  }

  test("P07: flatten(l: List[Any]): List[Any]") {

    assertThrows[IllegalArgumentException] {
      assert(flatten(null) == List())
    }

    assert(flatten(List()) == List(), "Should be List()")
    assert(flatten(List(List(1))) == List(1), "Should be List(1)")
    assert(flatten(List(List(1), List(2, 3))) == List(1, 2, 3), "Should be List(1,2,3)")
    assert(flatten(List(List(List(1), List(2, 3)))) == List(1, 2, 3), "Should be List(1,2,3)")
    assert(flatten(List(List(List(1), List(2, 3), 4), 5)) == List(1, 2, 3, 4, 5), "Should be List(1,2,3,4,5)")
    assert(flatten(List(0, List(0, List(1), List(2, 3), 4), 5)) == List(0, 0, 1, 2, 3, 4, 5), "Should be List(1,2,3,4,5)")
    assert(flatten(List('a, List(0, List("a"), List(2, 3), 4), 5)) == List('a, 0, "a", 2, 3, 4, 5), "Should be List(1,2,3,4,5)")
  }

  test("P08: compress(ts: List[Any]): List[Any]") {

    assertThrows[IllegalArgumentException] {
      assert(compress(null) == List())
    }
    assert(compress(List()) == List(), "Should be List()")
    assert(compress(List('a, 'a, 'a)) == List('a), "Should be List('a)")
    assert(compress(List('a, 'b, 'b)) == List('a, 'b), "Should be List('a, 'b)")
    assert(compress(List('a, 'a, 'b, 'b)) == List('a, 'b), "Should be List('a, 'b)")
    assert(compress(List('a, 'a, 'b, 'b, 'a, 'b, 'b)) == List('a, 'b, 'a, 'b), "Should be List('a, 'b, 'a, 'b)")
    assert(compress(List(1, 'a, 'b, 'b, 'a, 'b, 2)) == List(1, 'a, 'b, 'a, 'b, 2), "Should be List(1, 'a, 'b, 'a, 'b, 2)")
  }

  test("P09: pack(ts: List[Any]): List[Any]") {

    assertThrows[IllegalArgumentException] {
      assert(compress(null) == List())
    }
    assert(pack(List()) == List(), "Should be List()")
    assert(pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) ==
      List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)),
      "List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)")
    assert(pack(List('a, 'a, 'a, 'a, "h", 'c, 'c, 'a, 'a, 'd, 3, 'e, 'e, 'e)) ==
      List(List('a, 'a, 'a, 'a), List("h"), List('c, 'c), List('a, 'a), List('d), List(3), List('e, 'e, 'e)),
      """List(List('a, 'a, 'a, 'a), List("h"), List('c, 'c), List('a, 'a), List('d), List(3), List('e, 'e, 'e))""")
  }

  def encodeTests(e: (List[Any]) => List[(Int, Any)]) {
    assertThrows[IllegalArgumentException] {
      assert(e(null) == List())
    }
    assert(e(List()) == List(), "Should be List()")
    assert(e(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) ==
      List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
    assert(e(List('a, 'a, 'a, 'a, "h", 'c, 'c, 'a, 'a, 'd, 3, 'e, 'e, 'e)) ==
      List((4, 'a), (1, "h"), (2, 'c), (2, 'a), (1, 'd), (1, 3), (3, 'e)))

  }

  test("P10: encode(as: List[Any]): List[(Int, Any)] ") {
    encodeTests(encode)
  }

  test("P11: encodeModified[T](ts: List[List[T]]): List[Any] ") {

    assertThrows[IllegalArgumentException] {
      assert(encodeModified(null) == List())
    }
    assert(encodeModified(List()) == List(), "Should be List()")
    assert(encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) ==
      List((4, 'a), 'b, (2, 'c), (2, 'a), 'd, (4, 'e)))
    assert(encodeModified(List('a, 'a, 'a, 'a, "h", 'c, 'c, 'a, 'a, 'd, 3, 'e, 'e, 'e)) ==
      List((4, 'a), "h", (2, 'c), (2, 'a), 'd, 3, (3, 'e)))

  }

  test("P12: decode(as: List[(Int, Any)]): List[Any] ") {

    def doTests(d: List[(Int, Any)] => List[Any]) {
      assertThrows[IllegalArgumentException] {
        assert(d(null) == List())
      }
      assert(d(List()) == List(), "Should be List()")
      assert(d(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) ==
        List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
      )
      assert(d(List((4, 'a), (1, "h"), (2, 'c), (2, 'a), (1, 'd), (2, 3), (3, 'e))) ==
        List('a, 'a, 'a, 'a, "h", 'c, 'c, 'a, 'a, 'd, 3, 3, 'e, 'e, 'e))
    }

    doTests(decode)
    doTests(decodeB)
  }

  test("P13: encodeDirect(as: List[Any]): List[(Int, Any)] ") {
    encodeTests(encodeDirect)
  }

  test("P14: duplicate(as: List[Any]): List[Any] ") {

    assertThrows[IllegalArgumentException] {
      assert(duplicate(null) == List())
    }
    assert(duplicate(List()) == List(), "Should be List()")
    assert(duplicate(List('a)) == List('a, 'a))
    assert(duplicate(List('a, 'b, 'c, 'd, 'e)) == List('a, 'a, 'b, 'b, 'c, 'c, 'd, 'd, 'e, 'e))
    assert(duplicate(List('a, 'b, 1, "h")) == List('a, 'a, 'b, 'b, 1, 1, "h", "h"))
  }

  test("P15: duplicateN[T](n: Int, ts: List[T]): List[T] ") {

    // this is a special eta expansion
    val dN = duplicateN(_ : Int,_ : List[Any])
    val dN2 = duplicateN2(_ : Int,_ : List[Any])

    var fList = List(dN, dN2)

    fList.foreach(f => {
      assertThrows[IllegalArgumentException] {
        assert(f(0, null) == List())
      }
      assertThrows[IllegalArgumentException] {
        assert(f(-1, List()) == List(), "Should be List()")
      }
      assert(f(2, List()) == List(), "Should be List()")
      assert(f(3, List('a)) == List('a, 'a, 'a))
      assert(f(2, List('a, 'b, 'c, 'd, 'e)) == List('a, 'a, 'b, 'b, 'c, 'c, 'd, 'd, 'e, 'e))
      assert(f(2, List('a, 'b, 1, "h")) == List('a, 'a, 'b, 'b, 1, 1, "h", "h"))
      assert(f(1, List('a, 'b, 1, "h")) == List('a, 'b, 1, "h"))
    })
  }

  test("P16: drop(n: Int, as: List[Any]): List[Any] ") {

    assertThrows[IllegalArgumentException] {
      assert(drop(2, null) == List())
    }
    assertThrows[IllegalArgumentException] {
      assert(drop(-1, List()) == List(), "Should be List()")
    }
    assert(drop(2, List()) == List(), "Should be List()")
    assert(drop(3, List('a)) == List('a))
    assert(drop(1, List('a)) == List())
    assert(drop(3, List('a, 'a, 'a)) == List('a, 'a))
    assert(drop(2, List('a, 'b, 'c, 'd, 'e)) == List('a, 'c, 'e))
    assert(drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) ==
      List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
    assert(drop(3, List('a, 2, 'c, 'd, 'e, 'f, "g", 'h, 'i, 'j, 'k)) ==
      List('a, 2, 'd, 'e, "g", 'h, 'j, 'k))
  }

  test("P17: split[T](n: Int, as: List[T]): List[List[T]]") {

    assertThrows[IllegalArgumentException] {
      assert(split(2, null) == (List(), List()))
    }

    assertThrows[IllegalArgumentException] {
      assert(split(-1, List()) == (List(), List()), "Should be List()")
    }

    assert(split(2, List()) == (List(), List()), "Should be List()")
    assert(split(3, List('a)) == (List('a), List()))
    assert(split(3, List('a, 'a, 'a)) == (List('a, 'a, 'a), List()))
    assert(split(2, List('a, 'a, 'a)) == (List('a, 'a), List('a)))

    assert(split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) ==
      (List('a, 'b, 'c), List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))

  }

  test("P18: slice(s: Int, e: Int, as: List[Any]): List[Any]") {

    assertThrows[IllegalArgumentException] {
      assert(slice(2, 3, null) == List())
    }

    assertThrows[IllegalArgumentException] {
      assert(slice(-2, 3, List()) == List())
    }

    assertThrows[IllegalArgumentException] {
      assert(slice(-4, -3, List()) == List())
    }

    assertThrows[IllegalArgumentException] {
      assert(slice(5, 2, List()) == List())
    }

    assert(slice(3, 4, List()) == List(), "Should be List()")
    assert(slice(0, 1, List('a)) == List('a))
    assert(slice(0, 1, List('a, 'b)) == List('a))
    assert(slice(0, 2, List('a, 'b, 'c)) == List('a, 'b))
    assert(slice(2, 3, List('a, 'b, 'c)) == List('c))
    assert(slice(1, 3, List('a, 'b, 'c)) == List('b, 'c))
    assert(slice(1, 2, List('a, 'b, 'c)) == List('b))

  }

  test("P19: rotate(n: Int, ts: List[Any]): List[Any]") {

    assertThrows[IllegalArgumentException] {
      assert(rotate(2, null) == List())
    }

    val list = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)

    assert(rotate(0, list) ==
      List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))

    assert(rotate(14, list) ==
      List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))

    assert(rotate(3, list) ==
      List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))

    assert(rotate(-2, list) ==
      List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))

  }

  test("P20: removeAt(i: Int, as: List[Any]): (List[Any], Any)") {

    assertThrows[IllegalArgumentException] {
      assert(removeAt(2, null) == (List(), 0))
    }

    assertThrows[IllegalArgumentException] {
      assert(removeAt(2, List(1)) == (List(), 0))
    }

    assertThrows[IllegalArgumentException] {
      assert(removeAt(-2, List(1)) == (List(), 0))
    }

    val list = List('a, 'b, 'c, 'd)

    assert(removeAt(0, list) == (List('b, 'c, 'd), 'a))
    assert(removeAt(3, list) == (List('a, 'b, 'c), 'd))
    assert(removeAt(2, list) == (List('a, 'b, 'd), 'c))

  }

}