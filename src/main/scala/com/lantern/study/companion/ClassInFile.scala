package com.lantern.study.companion

class ClassInFile {

  private val privateVal : Int = 2

  private def sayHello = println("say hello")

}

object ClassInFile {

  def apply () = new ClassInFile

  def main (a : Array[String]) : Unit = {
    // companion object has access to private variable or method
    ClassInFile().sayHello
    ClassInFile().privateVal
  }
}