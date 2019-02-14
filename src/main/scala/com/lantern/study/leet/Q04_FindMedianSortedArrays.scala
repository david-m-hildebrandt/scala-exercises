package com.lantern.study.leet

object Q04_FindMedianSortedArrays {

  object Solution {
    def findMedianSortedArrays(nums1: Array[Int], nums2: Array[Int]): Double = {

      val totalLength = nums1.length + nums2.length
      val numsAll = new Array[Int](totalLength)
//      println(numsAll.toList)

      var i1 = 0
      var i2 = 0

      for {
        i <- 0 to ((totalLength + 1) / 2)
      } {
  //      println(s"i1: $i1 i2: $i2")

        if (i1 < nums1.length && i2 < nums2.length) {
          // both under, both may advance
          if (nums1(i1) < nums2(i2)) {
            numsAll(i) = nums1(i1)
            i1 += 1
          } else if (nums1(i1) > nums2(i2)) {
            numsAll(i) = nums2(i2)
            i2 += 1
          } else {
            numsAll(i) = nums1(i1)
            i1 += 1
          }
        } else if (i1 < nums1.length) {
          numsAll(i) = nums1(i1)
          i1 += 1
        }
        else if (i2 < nums2.length) {
          numsAll(i) = nums2(i2)
          i2 += 1
        } else {
//          println("in here")
        }

//        println(numsAll.toList)

      }
      var res = 0.0
      val i = totalLength / 2
      if (totalLength % 2 == 0) {
//        println(s"i: $i")
        res = (numsAll(i-1) + numsAll(i)) / 2.0
      } else {
        res = numsAll(i)
      }
//      println(res)
      res
    }
  }

  def main(a: Array[String]): Unit = {
    //    Solution.findMedianSortedArrays(Array(1, 2), Array(3, 4))
    //    Solution.findMedianSortedArrays(Array(3, 4), Array(1, 2, 5))
    //    Solution.findMedianSortedArrays(Array(), Array(1, 2, 3))
    Solution.findMedianSortedArrays(Array(1, 2, 3), Array(4, 5))
  }

}
