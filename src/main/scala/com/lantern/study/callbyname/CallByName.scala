package com.lantern.study.callbyname

object CallByName {

  var value = 5

  def main(a: Array[String]): Unit = {
    callByName(value)
    value = 5
    callByValue(value)
  }


  def callByName(v: => Int): Unit = {
    println(s"callByName: v: before ${v}")
    value = 10
    println(s"callByName: v: after ${v}")
  }

  def callByValue(v : Int): Unit = {
    println(s"callByValue: v: before ${v}")
    value = 6
    println(s"callByValue: v: after ${v}")
  }


  def getValue = 5


}
