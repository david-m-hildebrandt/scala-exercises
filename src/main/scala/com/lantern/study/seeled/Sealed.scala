package com.lantern.study.seeled

sealed trait Colour

object Yellow extends Colour

object Green extends Colour

object Blue extends Colour

//sealed class FamilyMember // will give warning if not all cases are handled
class FamilyMember

object David extends FamilyMember

object Suzi extends FamilyMember

object Landon extends FamilyMember

object Seeled {

  def findMatch( c: Colour) = c match {
    case Green => println("Green")
    case Yellow => println("Yellow")
    case _ => println("Something Else")
  }


  def findMatch( fm: FamilyMember) = fm match {
    case David => println("David")
    case Suzi => println("Suzi")
    case _ => println("Something Else")
  }

  def main(a: Array[String]): Unit = {
    findMatch(Green)
    findMatch(Blue)

    findMatch(David)
    findMatch(Suzi)
    findMatch(Landon)

  }


}
