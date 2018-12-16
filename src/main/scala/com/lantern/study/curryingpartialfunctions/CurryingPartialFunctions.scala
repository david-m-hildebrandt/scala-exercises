package com.lantern.study.curryingpartialfunctions

object CurryingPartialFunctions {

  // a curried method, parameters are separated
  def add(x : Int) (y: Int) : Int = x + y

  // a partial function (which is a value) that uses the curried method
  // one argument has been passed already
  val add10 = add(10) _

  def main(a : Array[String]) : Unit = {

    // calling the curried method
    println(add(10)(2))
    // called the partial function
    println(add10(2))

  }

}
