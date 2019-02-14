package com.lantern.study.leet

object Q08_StringToInteger {

  object Solution {

    def myAtoi(str: String): Int = {
      """^\s*([-+]?)0*(\d+)+""".r.findFirstMatchIn(str) match {
        case None => 0
        case Some(m) => {
          val sign = m.group(1)
          val value = m.group(2)
          val output = try {
            if (sign == "-") -value.toInt else value.toInt
          } catch {
            case e: NumberFormatException => if (sign == "-") Int.MinValue else Int.MaxValue
          }
          output
        }
      }
    }

  }

  def main(a: Array[String]): Unit = {

    evaluateValue("5000")
    evaluateValue("1")
    evaluateValue("12")
    evaluateValue("-20")
    evaluateValue("+20")
    evaluateValue("a+20")
    evaluateValue("-20 A")
    evaluateValue("+20 B")
    evaluateValue("A -20 A")
    evaluateValue("B +20 B")
    evaluateValue("20287342")
    evaluateValue("20")
    evaluateValue(Int.MinValue.toString)
    evaluateValue((Int.MinValue - 1L).toString)
    evaluateValue(Int.MaxValue.toString)
    evaluateValue((Int.MaxValue + 1L).toString)
  }

  def evaluateValue(value: String): Unit = {
    val result = Solution.myAtoi(value)
    println(s"value: $value result: $result")
  }
}
