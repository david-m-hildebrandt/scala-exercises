package com.lantern.study.questions

import scala.collection.mutable

/**
  * I have generated these questions for study purposes
  */
object Solutions {

  /**
    * Count letter occurrences in a String
    */

  def countChars(s : String) : List[(Char, Int)] = {

    val map = mutable.Map[Char,Int]()
    s.foreach(c => {
      map.put(c,  1 +  map.getOrElse(c, 0))
    })

    map.toList
  }


  /**
    * Map characters
    */

}
