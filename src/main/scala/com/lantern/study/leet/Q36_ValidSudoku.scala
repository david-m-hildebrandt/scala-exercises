package com.lantern.study.leet


object Q36_ValidSudoku {

  object Solution {

    import scala.collection.mutable
    import util.control.Breaks._

    def isValidSudoku(board: Array[Array[Char]]): Boolean = {
      var isValid = true
      breakable {
        for {
          i <- 0 until 9
        } {
          isValid &= isValidRow(board, i)
          if (!isValid) break
          isValid &= isValidColumn(board, i)
          if (!isValid) break
          isValid &= isValidSquare(board, i)
          if (!isValid) break
        }
      }
      isValid
    }

    def evaluate(map: mutable.Map[Char, Boolean], c: Char): Boolean = {
      var isValid = true
      breakable {
        map.put(c, false) match {
          case None => isValid &= true
          case Some(f) => {
            isValid &= c == '.'
            break
          }
        }
      }
      isValid
    }

    def isValidRow(board: Array[Array[Char]], row: Int): Boolean = {
      var map = mutable.Map[Char, Boolean]()
      var isValid = true
      for {
        column <- 0 until 9
      } {
        isValid &= evaluate(map, board(row)(column))
      }
      isValid
    }

    def isValidColumn(board: Array[Array[Char]], column: Int): Boolean = {
      var map = mutable.Map[Char, Boolean]()
      var isValid = true
      for {
        row <- 0 until 9
      } {
        isValid &= evaluate(map, board(row)(column))
      }
      isValid
    }

    def isValidSquare(board: Array[Array[Char]], i: Int): Boolean = {
      var map = mutable.Map[Char, Boolean]()
      var isValid = true
      val r = i / 3
      val c = i % 3
      val sr = r * 3
      val sc = c * 3

      for {
        row <- sr until sr + 3
        column <- sc until sc + 3
      } {
        isValid &= evaluate(map, board(row)(column))
      }
      isValid
    }
  }

  def main(a: Array[String]): Unit = {
    val board = SudoduInput.validInput
    val isValid = Solution.isValidSudoku(board)
    println(s"isValid : ${isValid}")
  }

}

object SudoduInput {
  val validInput = Array(
    Array('5', '3', '.', '.', '7', '.', '.', '.', '.'),
    Array('6', '.', '.', '1', '9', '5', '.', '.', '.'),
    Array('.', '9', '8', '.', '.', '.', '.', '6', '.'),
    Array('8', '.', '.', '.', '6', '.', '.', '.', '3'),
    Array('4', '.', '.', '8', '.', '3', '.', '.', '1'),
    Array('7', '.', '.', '.', '2', '.', '.', '.', '6'),
    Array('.', '6', '.', '.', '.', '.', '2', '8', '.'),
    Array('.', '.', '.', '4', '1', '9', '.', '.', '5'),
    Array('.', '.', '.', '.', '8', '.', '.', '7', '9')
  )

}
