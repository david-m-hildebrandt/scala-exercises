
package com.lantern.study.com.lantern.pattern

import java.lang.reflect.Field

class Avro

case class CreditData(var id: String, var name: Name, var address: List[Address], var fis: List[FI]) extends Avro

case class Name(var first: String, var last: String) extends Avro

case class Address(var number: Int, var streetName: String) extends Avro

case class FI(var name: String, var trades: List[Trade]) extends Avro

case class Trade(var an: Int) extends Avro

class ClassTagTypeTag {

  val tokenizer = Map(
    "com.lantern.study.com.lantern.pattern.Name.first" -> "first",
    "com.lantern.study.com.lantern.pattern.Name.last" -> "last",
    "com.lantern.study.com.lantern.pattern.Address.number" -> 0,
    "com.lantern.study.com.lantern.pattern.Address.streetName" -> "streetName",
    "com.lantern.study.com.lantern.pattern.FI.name" -> "fi",
    "com.lantern.study.com.lantern.pattern.Trade.an" -> 0
  )

}


object ClassTagTypeTag extends ClassTagTypeTag {

  def evaluateObject[O](o: O): Unit = {
    o.getClass.getDeclaredFields.foreach(
      f => {
   //     println(s"Checking: ${f.getName}")
        f.setAccessible(true)
        fieldCheck(f, o, f.get(o))
      }
    )
  }

  def fieldCheck[P, O](f: Field, p: P, o: O): Unit = {
    o match {
      case l: List[_] => l.foreach(evaluateObject(_))
      case a: Avro => evaluateObject(a)
      case _ => {
        val fieldName = f.getDeclaringClass.getTypeName + "." + f.getName
  //      println(fieldName)

        f.setAccessible(true)

        tokenizer.get(fieldName) match {
          case Some(v) => {
//            println(s"found: ${v} of type: ${v.getClass.getTypeName}")
            f.setAccessible(true)
//            val before = f.get(p)
            f.set(p, v)
//            val after = f.get(p)
  //          println(s"Before: ${before} After: ${after}")
          }
          case None =>
        }
//        println(o.getClass.getName)
      }

    }
  }


  def main(a: Array[String]): Unit = {

    val name = Name("David", "Hildebrandt")
    val addressNow = Address(83, "Fuller Ave")
    val addressBefore = Address(57, "Cavell Ave")
    val tradeBmoNow = Trade(23)
    val tradeBmoBefore = Trade(24)
    val tradeTdNow = Trade(71)
    val tradeTdBefore = Trade(78)

    val creditData = CreditData(
      "1",
      name,
      List(addressNow, addressBefore),
      List(
        FI("BMO", List(tradeBmoNow, tradeBmoBefore)),
        FI("TD", List(tradeTdNow, tradeTdBefore))
      )
    )

    println(creditData)
    evaluateObject(creditData)
    println(creditData)
  }

}

