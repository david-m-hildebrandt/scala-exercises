package com.lantern.study.typeclasstag

import scala.reflect.ClassTag
import scala.reflect.runtime.universe._ // for TypeTag

object TypeClassTag {

  // implicit is NOT required here : (implicit tag: ClassTag[T])
  // the compiler will add it since a Type parameter is used [T]
  def getValuesWithClassTag[T](list: List[Any])(implicit tag: ClassTag[T]): List[T] =
    list.flatMap {
      //      case (e @ tag(_:T)) => Some(e) // does not work with List[Any]
      case e: T => Some(e)
      case _ => None
    }


  // implicit is NOT required here

  def showType[T](v: T)(implicit tag: TypeTag[T]): String =
    tag.tpe match {
      case TypeRef(uType, uSym, args) =>
        List(uType, uSym, args).mkString("\n")
    }

  def main(a: Array[String]): Unit = {
    println("TypeClassTag")
    println(getValuesWithClassTag[Int](List("one", 1, 2, "two")))
    showType[List[Any]](List("one", 1, 2, "two"))
  }

}
