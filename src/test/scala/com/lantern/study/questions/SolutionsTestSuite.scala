package com.lantern.study.questions

import com.lantern.study.questions.Solutions._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SolutionsTestSuite extends FunSuite {

  test("countChars: ") {

    val s1 = "abcdeabcde"

    assert(countChars(s1).contains(('a', 2)))
    println(countChars(s1))

  }
}
