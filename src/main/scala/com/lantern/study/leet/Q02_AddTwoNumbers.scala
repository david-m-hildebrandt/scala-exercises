package com.lantern.study.leet

object Q02_AddTwoNumbers {

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


}
