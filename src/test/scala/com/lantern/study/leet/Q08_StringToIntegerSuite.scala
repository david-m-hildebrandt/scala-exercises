package com.lantern.study.leet

import com.lantern.study.leet.Q08_StringToInteger.Solution
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Q08_StringToIntegerSuite extends FunSuite {

  test("0 cases") {
    assert(Solution.myAtoi("abc") == 0)
    assert(Solution.myAtoi("abc -10") == 0)
    assert(Solution.myAtoi("abc 10") == 0)
    assert(Solution.myAtoi("abc -10 abc") == 0)
    assert(Solution.myAtoi("abc 10 abc") == 0)
  }

  test("Actual value cases") {
    assert(Solution.myAtoi("12") == 12)
    assert(Solution.myAtoi("-12") == -12)
  }

  test("Edge conditions and beyond") {
    assert(Solution.myAtoi("-2147483647") == -2147483647)
    assert(Solution.myAtoi("-2147483648") == -2147483648)
    assert(Solution.myAtoi("-2147483649") == -2147483648)

    assert(Solution.myAtoi("2147483646") == 2147483646)
    assert(Solution.myAtoi("2147483647") == 2147483647)
    assert(Solution.myAtoi("2147483648") == 2147483647)
  }

  test("Far beyond") {
    assert(Solution.myAtoi("10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000522545459") == 2147483647)
    assert(Solution.myAtoi(" 0000000000012345678") == 12345678)
  }
}
