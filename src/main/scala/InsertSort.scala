/**
  * Created by David on 2016-11-10.
  */
object InsertSort {

  def insertSort(xs: List[Int]): List[Int] = {
    if (xs.isEmpty) {
      println("insertSort: xs: " + xs)
      Nil
    }
    else {
      println("insertSort: calling insert(" + xs.head + ", insertSort(" + xs.tail + "))")
      insert(xs.head, insertSort(xs.tail))
    }
  }

  def insert(x: Int, xs: List[Int]): List[Int] = {
    println("   insert: x: " + x + " xs: " + xs)
    if (xs.isEmpty || x <= xs.head) {
      println("   insert: xs.isEmpty: " + xs.isEmpty + " x <= xs.head: " + (if (!xs.isEmpty) {
        (x <= xs.head)
      }) + " x::xs: " + (x :: xs))
      x :: xs
    }
    else {
      println("   insert: xs.head :: insert(x,xs.tail) : " + xs.head + " :: insert(" + x + ", " + xs.tail + ")")
      xs.head :: insert(x, xs.tail)
    }
  }

  def main(as: Array[String]): Unit = {
    val is = List(5, 6, 0, 2, 1, 4, 8, 7, 3, 9)
    val outcome = insertSort(is)

    println("------------------")
    println(outcome)

  }

}
