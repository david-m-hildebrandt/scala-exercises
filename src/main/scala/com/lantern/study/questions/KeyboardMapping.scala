package com.lantern.study.questions


abstract class Transformation(grid: Grid) {
  def transform(i: Int): Int

  def currentCoordinates(i: Int, grid: Grid): (Int, Int) = {
    (i % grid.numCols, i / grid.numCols)
  }
}

case class Vertical(grid: Grid) extends Transformation(grid) {
  override def transform(i: Int): Int = {
    val (col, row) = currentCoordinates(i, grid)
    val newRow = (grid.numRows - 1) - row
    val newIndex = (newRow * grid.numCols) + col
    newIndex
  }
}

case class Horizontal(grid: Grid) extends Transformation(grid) {
  override def transform(i: Int): Int = {
    val (col, row) = currentCoordinates(i, grid)
    val newCol = (grid.numCols - 1) - col
    val newIndex = (row * grid.numCols) + newCol
    newIndex
  }
}

case class Shift(grid: Grid, amount: Int) extends Transformation(grid) {
  override def transform(i: Int): Int = {
    val amountMod = amount % grid.ca.length
    val amountAbs = if (amountMod < 0) grid.ca.length + amountMod else amountMod
    val newIndex = (i + amountAbs) % grid.ca.length
    newIndex
  }
}

// uniqueness constraint

case class Grid(ca: Array[Char], numCols: Int, numRows: Int) {
  require(ca.length == numCols * numRows,
    s"Number of columns ($numCols) and rows ($numRows)does not result in size (${ca.length})of grid.")

  def findIndex(c: Char): Int = {
    val index = for (
      i <- ca.indices
      if ca(i) == c
    ) yield i
    if (index.size == 1) index(0)
    else throw new IllegalArgumentException(s"""Character '$c' does not exist in grid""")
  }


}


object KeyboardMapping {

  def calculateNetTransformation(o: Int, i: Int, grid: Grid, transformations: List[Transformation]): Transformation = {
    //    println(s"calculateNetTransformation: o: $o i: $i")
    // assumeds a shift is the correct notion
    transformations match {
      case Nil => Shift(grid, 0)
      case t :: Nil => Shift(grid, o - t.transform(i))
      case t :: tail => calculateNetTransformation(o: Int, t.transform(i), grid, tail)
    }
  }

  def evaluateTransformations(grid: Grid, transformations: List[Transformation]): Unit = {
    for {
      c <- grid.ca
    } {
      val i = grid.findIndex(c)
      val j = calculateNetTransformation(i, i, grid, transformations).transform(i)
      val net = if ((i - j) < 0) grid.ca.length + (i - j) else i - j
      println(s" c: $c i: $i j: $j net: $net")
    }
  }


  def main(a: Array[String]): Unit = {
    println("Hello")

    val ca = new Array[Char](8)
    val grid = Grid(ca, 2, 4)

    val testArray = Array[Char]('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h' , 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p','q', 'r', 's', 't', 'u', 'v', 'w', 'x')

    val testGrid = Grid(testArray, 6, 4)

    //    val transformations = List(Horizontal(testGrid))
    //    val transformations = List(Horizontal(testGrid), Horizontal(testGrid), Shift(testGrid, 3), Vertical(testGrid), Vertical(testGrid))
    //    val ts1 = List(Horizontal(testGrid), Horizontal(testGrid), Shift(testGrid, 3), Vertical(testGrid))
    //    val ts2 = List(Shift(testGrid, 3), Vertical(testGrid))
    //    val ts2 = List(Horizontal(testGrid), Shift(testGrid, 3), Horizontal(testGrid), Vertical(testGrid))

    //    val ts1 = List(Horizontal(testGrid), Shift(testGrid, 3), Horizontal(testGrid), Vertical(testGrid))
    //    val ts2 = List(Horizontal(testGrid), Shift(testGrid, 2), Horizontal(testGrid), Shift(testGrid, 1), Vertical(testGrid))
    //    val ts1 = List(Horizontal(testGrid), Vertical(testGrid), Horizontal(testGrid), Vertical(testGrid))
    //    val ts1 = List(Horizontal(testGrid), Vertical(testGrid), Vertical(testGrid), Horizontal(testGrid))
    // val ts2 = List(Horizontal(testGrid), Vertical(testGrid), Horizontal(testGrid), Vertical(testGrid), Vertical(testGrid), Vertical(testGrid))


    val ts1 = List(
      Horizontal(testGrid),
      Vertical(testGrid),
      Horizontal(testGrid),
      Shift(testGrid, 15),
      Horizontal(testGrid),
      Vertical(testGrid),
      Shift(testGrid, -12),
      Vertical(testGrid),
      Horizontal(testGrid),
      Vertical(testGrid),
      Vertical(testGrid),
      Horizontal(testGrid),
      Horizontal(testGrid),
      Horizontal(testGrid),
      Shift(testGrid, -100),
      Shift(testGrid, 58),
      Horizontal(testGrid)
    )

    val ts2 = List(
      Vertical(testGrid),
      Shift(testGrid, 15),
      Horizontal(testGrid),
      Vertical(testGrid),
      Shift(testGrid, -12),
      Vertical(testGrid),
      Shift(testGrid, -42),
      Horizontal(testGrid)
    )


    println("Evaluate Start")
    evaluateTransformations(testGrid, ts1)
    println("Evaluate ===")
    evaluateTransformations(testGrid, ts2)
    println("Evaluate Stop")


  }

}
