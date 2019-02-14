package com.lantern.study.leet

import scala.collection.mutable

object Q03_LongestSubstringWithoutRepeatedChars {

  object Solution {

    def lengthOfLongestSubstring2(s: String): Int = {

      def evaluateSubstring(longest: String, current: String, s: String): String = {
        if (s.isEmpty) {
          if (longest.length >= current.length) longest
          else current
        } else {
          val nc = if (current.contains(s.head)) {
            current.splitAt(current.lastIndexOf(s.head) + 1)._2 + s.head
          } else {
            current + s.head
          }
          if (nc.length > longest.length) {
            evaluateSubstring(nc, nc, s.tail)
          } else {
            evaluateSubstring(longest, nc, s.tail)
          }
        }
      }


      evaluateSubstring("", "", s).length
    }

    def lengthOfLongestSubstring(s: String): Int = {
      var max = 0;
      var i = 0
      var cMap: mutable.Map[Char, Int] = mutable.Map[Char, Int]()
      for {
        j <- 0 until s.length
      } {
        cMap.get(s.charAt(j)) match {
          case Some(v) => i = Math.max(v, i)
          case None => ()
        }
        max = Math.max(max, j - i + 1)
        cMap.put(s.charAt(j), j + 1)
      }
      max
    }
  }

  def main(a: Array[String]) = {
    println(Solution.lengthOfLongestSubstring("abcabcd"))
    ()
  }

}