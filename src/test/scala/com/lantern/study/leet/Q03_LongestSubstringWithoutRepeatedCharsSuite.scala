package com.lantern.study.leet

import com.lantern.study.leet.Q03_LongestSubstringWithoutRepeatedChars.Solution
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Q03_LongestSubstringWithoutRepeatedCharsSuite extends FunSuite {

  test("empty cases") {
    assert(Solution.lengthOfLongestSubstring("abc") == 3)
    assert(Solution.lengthOfLongestSubstring("abca") == 3)
    assert(Solution.lengthOfLongestSubstring("abcab") == 3)
    assert(Solution.lengthOfLongestSubstring("abcabc") == 3)
    assert(Solution.lengthOfLongestSubstring("abcabcd") == 4)
    assert(Solution.lengthOfLongestSubstring("abcdeabcd") == 5)
    assert(Solution.lengthOfLongestSubstring("aabbccddee") == 2)
    assert(Solution.lengthOfLongestSubstring("a") == 1)
    assert(Solution.lengthOfLongestSubstring("abb") == 2)
    assert(Solution.lengthOfLongestSubstring("aab") == 2)
    assert(Solution.lengthOfLongestSubstring("abcabcbbabcd") == 4)
  }


}

