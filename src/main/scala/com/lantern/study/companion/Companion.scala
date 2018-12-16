package com.lantern.study.companion


abstract class Gender {
  def toString: String
}

object Male extends Gender {
  override def toString() = "Male"
}

object Female extends Gender {
  override def toString() = "Female"
}

object NeitherMaleNorFemale extends Gender {
  override def toString() = "NeitherMaleNorFemale"
}

// this is not a case class
class Companion(name: String, var gender: Gender) {

  def getName: String = name

  def getGender: Gender = gender
}


object Companion {
  def apply(name: String, gender: Gender) = new Companion(name, gender)

  // the unapply method could be defined any way
  def unapply(companion: Companion) = Some(companion.getName, companion.getGender)
}

object CompanionStudy {

  def main(a: Array[String]): Unit = {
    val larry = Companion("Larry", Male)
    val suzi = Companion("Suzi", Female)
    val bc = Companion("BC", NeitherMaleNorFemale)

    val companions = List(larry, suzi, bc)

    companions.foreach(
      c => c match {
        // this form of 'Companion(x,Male)' will call 'unapply' above
        case Companion(x, Male) => println(s"Found a Male: ${x}")
          // never called, matched once already
        case Companion("Larry", Male) => println(s"Found Larry")
        case Companion("BC", gender) => println(s"BC is a: ${gender}")
        case _ => ()
      }
    )

    val Companion(name, gender) = suzi
    println(s"It seems Suzi is a ${gender}")

  }

}
/*
object ClassInFile {

  def apply () = new ClassInFile

  def main (a : Array[String]) : Unit = {
    // companion object has access to private variable or method, if in the same file
    //ClassInFile().sayHello // inaccessible here, must be in same file
    //ClassInFile().privateVal // inaccessible here, must be in same file
  }
}
*/