package com.lantern.study.leet



object LeetCode {


  def lengthOfLongestSubstring(s: String): Int = {
    import scala.collection.mutable

    def findLongestSubstring(s: List[Char], current: mutable.LinkedHashMap[Char, Char], longest: mutable.LinkedHashMap[Char, Char]): Int = {
      println(s"Entering: ${s} current: ${current} longest: ${longest}")
      s match {
        case Nil =>
          longest.size
        case c :: tail =>
          if (current.contains(c)) {
            val newCurrent = current.dropWhile { kv => kv._1 != c }.asInstanceOf[mutable.LinkedHashMap[Char, Char]]
            findLongestSubstring(tail, newCurrent, longest)
          }
          else {
            val newCurrent = (current + (c -> c)).asInstanceOf[mutable.LinkedHashMap[Char, Char]]
            if (newCurrent.size >= longest.size) {
              findLongestSubstring(tail, newCurrent, newCurrent)
            }
            else {
              findLongestSubstring(tail, newCurrent, longest)
            }
          }
      }
    }

    findLongestSubstring(s.toCharArray().toList, mutable.LinkedHashMap[Char, Char](), mutable.LinkedHashMap[Char, Char]())
  }


  def main(a: Array[String]): Unit = {
    //    println(lengthOfLongestSubstring("abcdefgaabcd"))
    //    println(lengthOfLongestSubstring("abcdaabcdefg"))
    //    println(lengthOfLongestSubstring(""))
    //    println(lengthOfLongestSubstring("a"))
    //    println(lengthOfLongestSubstring("aba"))
    //    println(lengthOfLongestSubstring("abac"))
    println(lengthOfLongestSubstring("dvdf"))
  }


}
