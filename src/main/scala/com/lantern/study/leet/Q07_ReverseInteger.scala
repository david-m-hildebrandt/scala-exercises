package com.lantern.study.leet

object Q07_ReverseInteger {

  object Solution {
    def reverse(x: Int): Int = {
      try {
        if (x > 0) x.toString.reverse.toInt
        else -x.abs.toString.reverse.toInt
      } catch {
        case e: NumberFormatException => 0
      }
    }
  }

}
