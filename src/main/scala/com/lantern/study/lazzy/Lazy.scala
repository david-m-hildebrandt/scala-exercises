package com.lantern.study.lazzy

object Lazy {

  lazy val xLazy: Int = {
    println("xLazy")
    1
  }

  lazy val yLazy: Int = {
    println("yLazy")
    1
  }

  val xNotLazy: Int = {
    println("xNotLazy")
    2
  }


  def calculate(a: Int, b: Int): Int = {
    a + b
  }

  def calculateByName(a: => Int, b: Int): Int = {
    a + b
  }


  lazy val b1L: Boolean = {
    println("b1L");
    false
  }
  lazy val b2L: Boolean = {
    println("b1L");
    false
  }

  def m1: Boolean = {
    println("f1");
    false
  }

  def m2: Boolean = {
    println("f2");
    false
  }


  def checkVal(b: Boolean) = println(b)

  def checkVal(b: Boolean, b2: Boolean) = println(b)

  def checkMethod(b: Boolean) = println(b)

  def checkMethod(b: Boolean, b2: => Boolean): Unit = println(b)

  def main(a: Array[String]): Unit = {
    println("calculate(xLazy, xLazy)")
    calculate(xLazy, xLazy)
    println("calculate(xNotLazy, xNotLazy)")
    calculate(xNotLazy, xNotLazy)
    println("calculate(xNotLazy, xLazy)")
    calculateByName(xNotLazy, yLazy)

    println("checkVal(b1L && b2L)")
    checkVal(b1L && b2L)

    println("checkMethod(m1 && m2)")
    checkMethod(m1 && m2)

    println("checkMethod(m1, m2)")
    checkMethod(m1, m2)

  }

}
