package com.lantern.study.caseclass

object CaseClassStudy extends App {

  class Fruit(c: String, m: Int) {

    // private
    private val mass: Int = m

    // public by default
    protected val colour: String = c

    // public by default
    // public by default
    def getColour: String = colour

    // demonstrates that the mass is treated separately as it is private
    def getMass: Int = mass + 10

  }

  // does not extend class Fruit
  object Fruit {
    def apply(colour: String, mass: Int) = new Fruit(colour, mass)
  }

  // a Case Class requires a parameter list
  // case class Apple extends Fruit
  case class Apple(colourIn: String, massIn: Int) extends Fruit(colourIn, massIn)

  // 'colourIn' is bound to the case class,
  // 'mass' is private, so may be passed in, there is a 'mass' in Orange and Fruit, 2 instances of Int
  case class Orange(colourIn: String, segmentNumber: Int, var mass: Int) extends Fruit(colourIn, mass) {
    // sees colour, as it is protected
    def printColour = println(this.colour)

    def printMass = println(s"Orange.mass: ${mass.hashCode()} Fruit.getMass: ${this.getMass.hashCode()}")
  }

  // 'colour' as a variable, is part of the hierarchy, so 'override' must be used
  // as also must be 'val' or 'var'
  case class Banana(override val colour: String, mass: Int) extends Fruit(colour, mass)

  case class Pear(override val colour: String, protected val mass: Int, private val variety: String) extends Fruit(colour, mass) {
    def getVariety: String  = variety
  }

  // also an example of composition of case classes
  case class FruitBowl(fruit: List[Fruit])

  override def main(a: Array[String]): Unit = {

    val apple = Apple("red", 120)
    val orange = Orange("orange", 8, 80)
    val banana = Banana("yellow", 70)

    val fruitBowl = FruitBowl(List(apple, orange, banana))

    println("Contents of FruitBowl")
    fruitBowl.fruit.foreach(f => println(f.getColour))

    println(Apple("red", 20).getColour)

    println(Apple("red", 30).colourIn)

    orange.printMass

    orange.mass = 40

    orange.printMass

  }

}


