package com.lantern.study.cats

import cats.kernel.Semigroup

object SemigroupStudy {

  import cats.implicits._

  implicit val intTupleInIntTupleOut : Semigroup[(Int,Int) => (Int,Int)] = new Semigroup[(Int, Int) => (Int, Int)] {
    override def combine(x: (Int, Int) => (Int, Int), y: (Int, Int) => (Int, Int)): (Int, Int) => (Int, Int) = {
      val xr = x(_,_)
      val yr = y(_,_)
      xr.
    }

  }

  def main(a: Array[String]): Unit = {
    println("hello")
    println(Semigroup[List[Int]].combine(List(1, 2, 3), List(4, 5, 6)))
    assert(Semigroup[Int].combine(1, 2) == 3)

    // combining functions
    println(Semigroup[Int => Int].combine(_ + 1, _ * 10).apply(6) )
    println(Semigroup[(Int,Int) => (Int,Int)].combine((x :Int, y: Int) => (x, y)).apply(6) )

  }
}
