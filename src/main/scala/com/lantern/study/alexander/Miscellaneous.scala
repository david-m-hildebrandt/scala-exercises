package com.lantern.study.alexander

import scala.annotation.switch
import scala.collection.mutable.ArrayBuffer

object Miscellaneous {

  def doForLoops(): Unit = {

    // without yield, nothing is yielded, it is us iterated
    val s = "david hildebrandt"
    val a = for (
      c <- s.filter(_ != 'i')
      if c != 'a'
    ) yield {
      c
    }
    println(a.toUpperCase().mkString(""))

    //    s.filter(_ != 'a').foreach(println)

    val res = for (
      c <- s.toUpperCase
    ) yield {
      c
    }

    println(res)

  }

  /**
    * Regular Expressions
    * Note the .r following the regular expression pattern
    *
    * val regEx = "[0-9]+".r
    *
    * Note also this construction
    * regEx.findAllIn(address)
    * regEx.findAllIn(address)
    *
    */

  def doStringRegex(): Unit = {

    val regEx = "[0-9]+".r

    val address = "24 Longview Ave, Blind River, P0R 1B0"

    regEx.findAllIn(address).foreach(x => println(s"Found ${x}"))

    regEx.findFirstIn(address) match {
      case None => println("Nothing found")
      case Some(v) => println(s"Found: ${v}")
    }
  }

  /**
    * The pattern has used groups, which may be used
    * in the unapply
    * pattern(number, subject)
    */
  def doPatternMatch: Unit = {

    val pattern = "([0-9]+) ([A-Za-z]+)".r

    // unapply is available on the regular expression construction
    val pattern(number, subject) = "101 Dalmations"

    println(s"Number: ${number}")
    println(s"Subject: ${subject}")

    // 101 - 53 Mount Pleasant Ave
    val pattern2 = "([0-9]+) [-]? ([0-9]+) ([A-Za-z ]+)".r
    val pattern2(unit, streetNumber, streetName) = "101 - 53 Mount Pleasant Ave"

    println(s"Unit: ${unit}")
    println(s"Number: ${streetNumber}")
    println(s"Street: ${streetName}")
  }

  def doStringsAsArrays: Unit = {
    val subject = "Landon Hildebrandt"
    for (i <- 0 until subject.length) print(subject(i))
    println
  }

  /**
    *
    *
    **/
  implicit class StringModifier(s: String) {
    def increment = s.map(c => (c + 1).toChar)

    def toInt(radix: Int) = Integer.parseInt(s, radix)
  }

  // Special case too
  implicit class NumberModifier(d: Double) {
    def ~=(other: Double): Boolean = if ((d - other).abs < 0.001) true else false
  }

  def doImplicitString: Unit = {
    println("HAL".increment)
    println("1010".toInt(2))
  }

  def doNumberValidityChecks: Unit = {
    println(1000.isValidByte)
    println(1000.isValidLong)
    println((10: Byte).isValidByte) // type follows 10:Byte
    val bd = 238: BigDecimal // type follows
    println(bd.isDecimalDouble)
    println(s"Hex is valid long: ${0xff.isValidLong}")

    val d1 = 0.1
    val d2 = 0.100003
    println(s"d1 + d2=${d1 + d2} and d1~=d2 is ${d1 ~= d2}")
    println(s"d1 - d2=${d1 - d2} and (d1-d2).abs is ${(d1 - d2).abs}")

  }

  def doGeneratorsAndGuards = {
    for (i <- 1 to 10 if i > 2 && i < 5) print(i)
    println
    for (i <- 1 to 2; j <- 1 to 2) println(s" i ${i} j ${j}")
  }

  def handleExceptions(op: => Unit) = {
    try {
      op
    } catch {
      //      case e: RuntimeException => println(e)
      case e: RuntimeException =>
    }
  }

  def doControlStructure: Unit = {

    handleExceptions(println(1))
    handleExceptions(println(1 / 0))
    handleExceptions(println(0))

    handleExceptions {
      println(2 / 0)
    }
  }


  def handle[A <: Any](f: A => A)(a: A): Option[A] = {
    try {
      Some(f(a))
    } catch {
      case e: RuntimeException => None
    }
  }





  def doSwitchDemo = {
    val i = 2

    // @switch is used to optimize the look-ups
    val out = (i: @switch) match {
      case 1 => "one"
      case 2 => "two"
    }

    println(out)
  }


  sealed trait Command

  object Go extends Command

  object Start extends Command

  object Stop extends Command

  object End extends Command

  case class Other(name: String) extends Command

  val jump = Other("Jump")

  def doCommandProcessing = {

    def process(cmd: Command) = cmd match {
      case Go | Start => println("Starting")
      case Stop | End => println("Ending")
      case other => println(other)
    }

    process(Go.asInstanceOf[Command])
    process(Stop)
    process(jump)
  }


  def doPatternMatchingAgain = {

    def matchIt[T](t: T) = t match {
      // v@T means alias v at matched value T
      case a@Array(1, 2) => println(s"Found array: Array(${a.mkString(",")})")
      case list@List(1) => println(s"List(1): ${list}")
      case list@List(1, _, 3) if list.contains(2) => println(s"List(1,2,3)")
      case List(a, _, _) => println("List(a,_,_)")
      case List(a, _*) => println("List(a,_*)")
      case x: List[_] => println("list")
      //      case x: Array[][] => println("Array[_][_]")
      //      case x: Array[_][_][_] => println("Array[_][_][_]")
      case _ => println("default")

    }

    matchIt(List())
    matchIt(List(1))
    matchIt(List(1, 2, 3))
    matchIt(Array(Array(Array(1))))
    matchIt(Array(Array(1)))
    matchIt(Array(1, 2))


  }

  def doAccessors = {

    class Constructor(name: String) {
      def getName() = name
    }

    class ValConstructor(val name: String)

    class VarConstructor(var name: String)

    // name is unavailable
    println(new Constructor("Landon").getName())
    // name is available
    println(new ValConstructor("Landon").name)
    //
    val landon = new VarConstructor("Landon")
    landon.name = "Daniel" // setter is accessible
    println(landon.name)

  }

  def doAuxilliaryConstructors = {

    class Congregation(val denomination: String, val size: Int) {

      def this(size: Int) = this("Catholic", size)

      def this(denomination: String) = this(denomination, 400)


    }

    println(new Congregation(300).denomination)
    println(new Congregation("Lutheran").size)

  }

  def doConstructorDefaults(): Unit = {

    class Default(var b: Int = 2000)

    println(new Default().b)

  }

  def doAbstractClassVersusTrait(): Unit = {

    // abstract may take a parameter
    // could not be mixed
    abstract class Top(name: String) {
      def doPart: Int
    }

    // trait cannot take a parameter
    // could be mixed
    trait Bottom

    case class Roof(name: String) extends Top("building") {
      override def doPart: Int = 0
    }
    object Gutter extends Bottom

  }

  def doDefiningEquals(): Unit = {

    case class Colour(name: String) {
      override def equals(other: Any): Boolean = {
        other.isInstanceOf[Colour] &&
          other.asInstanceOf[Colour].name.toUpperCase() == name.toUpperCase()
      }
    }

    println(Colour("black") == Colour("BLack"))
    println(Colour("black") == Colour("White"))

  }

  def doSuperInvocations = {

    trait Parent {
      def name = "Parent"
    }

    trait Child extends Parent {
      override def name: String = "Child"
    }

    trait GrandChild extends Child {
      override def name: String = "GrandChild"
    }

    class Example extends Parent with Child with GrandChild {
      def example = name

      def parent = super[Parent].name

      def child = super[Child].name

      def grandChild = super[GrandChild].name
    }

    val example = new Example()

    println(example.example)
    println(example.parent)
    println(example.child)
    println(example.grandChild)

  }

  def doTuple = ("one", 1)

  def doUseDoTuple = {
    println(doTuple._1)
  }

  def doVarArgAdaptation = {
    def printAll(a: Any*): Unit = {
      a.foreach(println)
    }

    printAll("a", 1)
    val l = List("a", 2)
    printAll(l: _*)
    val a = Array("a", 'c', 3, 2.3)
    // this will not expand the argument set
    printAll(a)
    // this will expand the argument set in the Array
    printAll(a: _*)
  }

  def doImportRenaming = {
    import scala.collection.immutable.{List => MyList}
    println(MyList("123abc"))
  }

  def doImporHiding = {
    import scala.collection.immutable.{List => _}
    println(Array("1"))
    println(List("2")) // this might be working because I have two scala libraries in the class path
  }

  def doTraitInheritanceControl = {

    class Catholic
    trait Priest {
      // this mean that 'this' MUST be used in a hierarchy that includes 'Catholic'
      this: Catholic =>
    }

    object FatherSabas extends Catholic with Priest
    //    object LittleJohn extends Priest // will not compile, not a Catholic

  }

  def doTraitInheritanceControlInMethod = {

    class Catholic {
      def saysPrayers = {

      }

      def takesConfession = {}
    }
    trait Priest {
      // this mean that 'this' MUST be used in a hierarchy that includes
      // these methods
      this: {
        def saysPrayers: Unit // return type not required
        def takesConfession // assumed to be Unit
      } =>
    }

    object FatherSabas extends Catholic with Priest
    //    object LittleJohn extends Priest // will not compile, not a Catholic

  }

  def doTraitCreationNotice = {
    // this is like some static initialization code
    trait TakeNotice {
      println(s"Look at me!  ${this}")
    }

    case class Me(name: String) extends TakeNotice
    Me("Hillary")
    ()
  }


  def doFunctionsAsVariables = {

  }

  object Solution {
    def twoSum(nums: Array[Int], target: Int): Array[Int] = {
      val result: Seq[(Int, Int)] = for {
        i <- 0 until nums.size
        j <- (i + 1) until nums.size
        if (nums(i) + nums(j) == target)
      } yield {
        (i, j)
      }

      println(s"result: ${result}")
      println(s"result: ${Array(result.head._1, result.head._2)}")
      Array(result.head._1, result.head._2)
    }

    class ListNode(var _x: Int = 0) {
      var next: ListNode = null
      var x: Int = _x
    }

    def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {


      def addTwoNumbers(l1: ListNode, l2: ListNode, carry: Int): ListNode = {

        if (l1 == null && l2 == null && carry > 0) {
          val ln = new ListNode(carry)
          ln.next = null
          ln
        } else if (l1 == null && l2 == null) null
        else if (l1 == null && l2 != null) {
          val value = l2.x + carry
          val newCarry = value - (value % 10)
          val newValue = value - newCarry

          val ln = new ListNode(newValue)
          ln.next = addTwoNumbers(l1, l2.next, newCarry / 10)
          ln
        }
        else if (l1 != null && l2 == null) {
          val value = l1.x + carry
          val newCarry = value - (value % 10)
          val newValue = value - newCarry

          val ln = new ListNode(newValue)
          ln.next = addTwoNumbers(l1.next, l2, newCarry / 10)
          ln
        } else {
          val value = l1.x + l2.x + carry
          val newCarry = value - (value % 10)
          val newValue = value - newCarry

          val ln = new ListNode(newValue)
          ln.next = addTwoNumbers(l1.next, l2.next, newCarry / 10)
          ln
        }
      }

      def showValues(listNode: ListNode): Unit = {
        var lN = listNode
        do {
          print(s"-${lN.x}")
          lN = lN.next
        } while (lN != null)
        println
      }


      val a2 = new ListNode(2)
      val a4 = new ListNode(4)
      val a3 = new ListNode(3)

      a2.next = a4
      a4.next = a3

      val b5 = new ListNode(5)
      val b6 = new ListNode(6)
      val b4 = new ListNode(4)

      b5.next = b6
      b6.next = b4

      showValues(a2)
      showValues(b5)

      val ls = addTwoNumbers(a2, b5, 0)

      showValues(ls)

      val c5 = new ListNode(5)
      val d5 = new ListNode(5)

      val cd = addTwoNumbers(c5, d5, 0)

      showValues(cd)

      null

    }
  }

  def reverse(x: Int): Int = {
    if (x < 0) {
      -BigInt((-1 * x).toString.reverse).toInt
    } else {
      BigInt(x.toString.reverse).toInt
    }

  }

  def doCallback(callback: => () => Unit) = {
    callback()
  }

  def doCallbackForInt(op: (Int) => Int, i: Int): Int = {
    op(i)
  }

  val sayHello: () => Unit = () => println("hello")

  val sum = (a: Int, b: Int) => a + b

  val sumPartial10: Int => Int = (b: Int) => sum(10, b)

  val sumPartial5: Int => Int = (b: Int) => sum(5, b)

  val sumPartial20 = sum(20, _: Int)

  def getPartialSum(base: Int): Int => Int = sum(base, _: Int)

  def doPartialStudy = {

    println(sum(1, 2))
    println(sumPartial10(2))
    println(sumPartial5(2))
    println(sumPartial20(2))
    val p30 = getPartialSum(30)
    println(p30(4))
  }


  def doClosureStudy() = {
    var acceptable = 155

    val tooFat = (w: Int) => w > acceptable

    def surveySays(q: Int => Boolean, w: Int) = {
      if (q(w)) println("You are too chunky")
      else println("Just right.")
    }

    surveySays(tooFat, 160)
    acceptable = 170
    surveySays(tooFat, 160)
  }


  val makeItADouble: (Int) => (Int) = _ * 2

  def doCollectionStudy = {

    val ab = ArrayBuffer('a', 'b', 'c', 'd')
    println(ab)
    ab(0) = 'e'
    println(ab)
    val v = Vector('a', 'b', 'c', 'd')
    println(v)
    val vp = v ++ Vector('k')
    println(vp)

    vp.foreach(println(_))
    val vn = for (v <- vp; if v != 'b') yield v
    vn.foreach(println(_))

    val m = Map("a" -> 1, "b" -> 2, "c" -> 3)
    m.foreach { case (letter, number) => println(s"letter: ${letter} number: ${number}") }
    for ((k, v) <- m) yield println(s"key: ${k} value: ${v}")

    for ((v, i) <- v.zipWithIndex) println(s"${v} ${i}")
    for ((v, i) <- v.zip(Stream from 1)) println(s"${v} ${i}")

    def showOperands(x: Int, y: Int): Int = {
      println(s"x: ${x} y: ${y} r: ${x - y}")
      x - y
    }

    val n = Vector(1, 2, 3)
    println(n.reduceLeft(_ * _))
    var r = 1 to 3
    println(r.reduceLeft(_ * _))
    println(r.reduceLeft(showOperands))
    var s = -3 to 3
    println(s.reduceLeft(showOperands))
    println(s.reduceRight(showOperands))

    println("Before xStrict")
    val xStrict = (0 to 100).map { i => Thread.sleep(10); i }
    println("Done xStrict")
    println("Before xLazy")
    val xLazy = (0 to 100).view.map { i => Thread.sleep(10); i }
    println("Done xLazy")

    println(List.range(0, 10, 2))
    println(List.range(0, 3))
    println(Vector.range(0, 3))
  }

  def doEnumerationStudy = {
    object Roommate extends Enumeration {
      type Roommate = Value
      val SUZI, LANDON, MICKEY = Value
    }

    println(Roommate.values)

  }

  def doTupleStudy = {

    val (x, y, z) = (1, 2, 3)

    val a = (4, 5, 6)
    val b = (4 -> 5 -> 6) // different pattern

    val (h, _, g) = a
    println(h)
    println(g)
    println(a._1)
    println(a._3)
    val ((i, _), j) = b // different pattern
    println(i)
    println(j)
    a.productIterator.foreach(e => println(s"found: ${e}"))

  }

  def doArrayStudy = {

    val (cols, rows) = (2, 3)
    val a = Array.ofDim[Int](cols, rows)

    for (c <- 0 until cols; r <- 0 until rows) a(c)(r) = c + r

    for (c <- 0 until cols; r <- 0 until rows) println(a(c)(r))

  }


  def doStreamStudy = {

    val s = Stream[Int]()
    val s1 = 1 #:: 2 #:: Stream.empty
    val sr = (0 to 1000).toStream
    println(sr(0))
    for {
      i <- 0 to 10
      if (i % 2 == 0)
    } println(sr(i))
    println(sr.take(10))

    lazy val fibs: Stream[BigInt] = BigInt(0) #:: BigInt(1) #:: fibs.zip(fibs.tail).map(n => n._1 + n._2)
    for {
      i <- 0 to 10
    } println(fibs(i))

    def square(v: BigInt): BigInt = v * v

    lazy val squareStream: Stream[(BigInt, BigInt)] = (BigInt(0), BigInt(0)) #:: squareStream.map(
      t => {
        val next = t._1 + 1
        (next, square(next))
      }
    )

    for {
      i <- 0 to 10
    } println(squareStream(i))

    def fibProdStream: Stream[BigInt] = {
      def fps(v1: BigInt, v2: BigInt): Stream[BigInt] = v1 #:: fps(v2, v1 * v2)

      fps(1, 2)
    }

    val fps = fibProdStream

    for {
      i <- 0 to 10
    } println(fps(i))

    val fpsi = fps.iterator


  }

  def main(a: Array[String]): Unit = {

    //    doForLoops
    //    doStringRegex()
    // doPatternMatch
    /*
    doStringsAsArrays
    doImplicitString
    doNumberValidityChecks
    doGeneratorsAndGuards
    doControlStructure
    doSwitchDemo
    doCommandProcessing
    */
    //doPatternMatchingAgain
    /*doAccessors
    doAuxilliaryConstructors
    doConstructorDefaults
    doDefiningEquals
    doSuperInvocations
    doUseDoTuple
    */
    doVarArgAdaptation
    /*doImportRenaming
    doTraitCreationNotice

    println(Solution.twoSum(Array(1, 9), 10))
    println(Solution.addTwoNumbers(null, null))

    println(reverse(321))
    println(reverse(320))
    println(reverse(-123))
    doCallback(sayHello)
    println(doCallbackForInt(makeItADouble, 3))
    println(List(1, 2, 3).map(makeItADouble))
    doClosureStudy()
    doPartialStudy
    doCollectionStudy
    doEnumerationStudy
    doTupleStudy
    doArrayStudy

*/
    //    doStreamStudy
  }

}

// must be a top level object for the IDE, at least
object AnotherMain extends App {
  println("AnotherMain!")
}
