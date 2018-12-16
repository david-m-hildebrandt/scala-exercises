package com.lantern.study.forcomprehension

object ForComprehension {

  case class Person(name: String, children: List[Person])

  val landon = Person("Landon", List.empty)
  val david = Person("David", List(landon))

  val holly = Person("Holly", List.empty)
  val anna = Person("Anna", List.empty)
  val brian = Person("David", List(holly, anna))

  val bruno = Person("Bruno", List(david, brian))

  case class Position(index: String, occupied: Option[Boolean], next: Option[Position])

  val p3 = Position("3", Some(true), None)
  val p2 = Position("2", Some(true), Some(p3))
  val p1 = Position("1", Some(true), Some(p2))

  def main(a: Array[String]): Unit = {

    val grandChildren = for {
      children <- bruno.children
      grandchildren <- children.children
    } yield {
      grandchildren
    }
    println(grandChildren)

    val gc = bruno.children.flatMap(c => c.children)

    println(gc)

    def getLast(p: Position, last: Position): Position = {
      p.next match {
        case None => p
        case Some(n) => getLast(n, p)
      }
    }

    println(getLast(p1, p1))

    val posFinal1 = p1.next.flatMap(n => n.next)

    println(posFinal1)

    val posFinal2 = for {
      n1 <- p1.next
      n2 <- n1.next
    } yield {
      n2
    }

    println(posFinal2)
  }


}
