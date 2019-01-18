package com.lantern.study.questions

import scala.collection.mutable

/**
  * I have generated these questions for study purposes
  */
object Solutions {

  /**
    * Count letter occurrences in a String
    */

  def countChars(s: String): List[(Char, Int)] = {

    val map = mutable.Map[Char, Int]()
    s.foreach(c => {
      map.put(c, 1 + map.getOrElse(c, 0))
    })

    map.toList
  }

  def main(a: Array[String]): Unit = {

    val letterMap = Map(
      'a' -> 4, 'A' -> 4,
      'e' -> 3, 'E' -> 3,
      'i' -> 1, 'I' -> 1,
      'o' -> 0, 'O' -> 0,
      's' -> 5, 'S' -> 5,
      't' -> 7, 'T' -> 7)

 //   println(stringMapper("hello", letterMap))
    println(countLetters2("aaabbbc"))
  }

  def stringMapper(input: String, letterMap: Map[Char, Int]): String = {
    input.map(c => {
      letterMap.getOrElse(c, c)
    }).mkString("")
  }


  def countLetters(s: String): String = {

    if (s == null || s == "") {
      ""
    }

    var counter: Int = 1
    var currentChar: String = s.substring(0, 1)
    var output: String = ""

    s.substring(1).foreach {
      c => {
        println(c)
        if (("" + c) == currentChar) {
          counter = counter + 1
        } else {
          output = output + currentChar + counter
          currentChar = "" + c
          counter = 1
        }
      }
    }

    output + currentChar + counter
  }


  def countLetters2(s: String): String = {

    if (s == null || s == "") {
      ""
    }

    var counter: Int = 1
    var currentChar: Char = s.substring(0, 1).toCharArray.head
    var output: String = ""

    s.substring(1).foreach {
      c => {
        if (c == currentChar) {
          counter = counter + 1
        } else {
          output = output + currentChar + counter
          currentChar = c
          counter = 1
        }
      }
    }

    output + currentChar + counter
  }

  def countLetters3(s: String): String = {

    if (s == null || s == "") {
      ""
    }

    var counter: Int = 1
    var currentChar: Char = s.substring(0, 1).toCharArray.head
    var output: String = ""

    s.substring(1).foreach {
      c => {
        if (c == currentChar) {
          counter = counter + 1
        } else {
          output = output + currentChar + counter
          currentChar = c
          counter = 1
        }
      }
    }

    output + currentChar + counter
  }

  /**
    * Map characters
    */

}
