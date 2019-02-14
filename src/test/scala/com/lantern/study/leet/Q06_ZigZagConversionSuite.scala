package com.lantern.study.leet

import com.lantern.study.leet.Q07_ReverseInteger.Solution
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Q06_ZigZagConversionSuite extends FunSuite {

  test("Zero cases") {
    assert(Solution.reverse(0) == 0)
  }

  test("Positive cases") {
    assert(Solution.reverse(12) == 21)
  }

  test("Negative cases") {
    assert(Solution.reverse(-12) == -21)
  }

  test("Overflow cases") {
    assert(Solution.reverse(Int.MaxValue) == 0)
    assert(Solution.reverse(Int.MinValue) == 0)
  }
}
