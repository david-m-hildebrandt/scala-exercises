package com.lantern.study.leet

import com.lantern.study.leet.Q06_ZigZagConversion.Solution
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Q07_ReverseIntegerSuite extends FunSuite {

  test("Zero cases") {
    assert(Solution.convert("ABC", 3) == "ABC")
  }

}
