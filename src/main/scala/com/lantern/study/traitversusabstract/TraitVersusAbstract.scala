package com.lantern.study.traitversusabstract

object TraitVersusAbstract {

  trait Mass[T] {
    val x = 1
    def getVal2 : T
  }

  abstract class Container[T] (name: String) {
    def toUpper = name.toUpperCase
    def getVal : T
  }

  case class Vessel(val name : String) extends Container[Int](name) {
    override def getVal : Int = 1
  }

  case class Vase(val name : String) extends Container[Int](name) with Mass[String] {
    override def getVal : Int = 1
    override def getVal2 : String = "one"
  }


  class AbstractTop(a : Int)

  case class BottomAbstractTop (a: Int) extends AbstractTop(a)

  case class Top(a : Int)
  // compiler error if defined
//  case class BottomTop (override val a: Int) extends Top(a)




  def main (a : Array[String]): Unit = {
    println(Vessel("vessel").getVal)
    println(Vase("vase").getVal2)
    println(Vessel("vessel"))
    println(BottomAbstractTop(2))
//    println(BottomTop(2)) // compiler error
  }


}
