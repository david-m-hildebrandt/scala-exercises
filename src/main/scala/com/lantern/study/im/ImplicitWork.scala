package com.lantern.study.im

import scala.reflect.ClassTag

case class Data(a: Int)

object ImplicitWork extends App {

  import scala.reflect.runtime.universe._

  // if t: T, compiler will not allow anything but a match
  // if t: Any, compiler allows any type to be used for comparison
  def matchClass[T: ClassTag](t: Any) : Unit = t match {
    case t: T => println(s"It's a ${t.getClass}")
    case _ => println("Not sure.")
  }

  // note that Manifest is deprecated
  def matchWithManifest[T](list: List[T])(implicit m: Manifest[T]) : Unit = {
    val fName = "matchWithManifest"
     if (m <:< manifest[String]) println(s"${fName} It's a String list: ${list.getClass}")
    if (m <:< manifest[Int]) println(s"${fName} It's a Int list: ${list.getClass}")
    if (m <:< manifest[Data]) println(s"${fName} It's a Data list: ${list.getClass}")
  }

  def identifyListTypeWithClassTag[T: ClassTag](list: List[T])(implicit tag: TypeTag[T]): Unit = list match {
    //  def identifyListTypeWithTypeTag[T](list: List[T]): Unit = list match {
    case ld: List[c] => println("List[Data]")
    case li: List[Int] => println("List[Int]")
    case ls: List[String] => println("List[String]")
  }


  def identifyListTypeWithTypeTag[T](list: List[T])(implicit tag: TypeTag[T]): Unit = list match {
//  def identifyListTypeWithTypeTag[T](list: List[T]): Unit = list match {
    case ld: List[Data] => println("List[Data]")
    case li: List[Int] => println("List[Int]")
    case ls: List[String] => println("List[String]")
  }

  def identifyListType[T](list: List[T]): Unit = list match {
    case ld: List[Data] => println(s"List[Data]: ${ld}")
    case li: List[Int] => println(s"List[Int]: ${li}")
    case ls: List[String] => println(s"List[String]: ${ls}")
  }

  def evaluate[T](f: List[T] => Unit)(list: List[T]): Unit = {
    f(list)
  }

  override def main(a: Array[String]): Unit = {
    val ld = List(Data(1), Data(2))
    val li = List(1, 2)
    val ls = List("1", "2")

    def idNoType = evaluate(identifyListType) _

    idNoType(ld)
    idNoType(li)
    idNoType(ls)

    identifyListType(ld)
    identifyListType(li)
    identifyListType(ls)

//    identifyListTypeWithTypeTag(ld)
//    identifyListTypeWithTypeTag(li)
//    identifyListTypeWithTypeTag(ls)

    matchClass[Int](1)
    matchClass[Int]("1")
    matchClass[String](1)
    matchClass[String]("1")


  }

}

